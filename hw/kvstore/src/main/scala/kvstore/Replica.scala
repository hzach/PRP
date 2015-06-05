package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

import scala.util.{Failure, Success, Random}

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  val persistence = context.actorOf(persistenceProps)
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  override def preStart(): Unit = {
    arbiter ! Join
  }

  def receive = {
    case JoinedPrimary   => context become leader
    case JoinedSecondary => context become replica(0)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    // Inserts the new key-value pair into the
    // store. A new entry is created if it does
    // not already exists and the current value is
    // overriden if it if present
    case Insert(key, value, id) =>
      kv = kv.updated(key, value)
      sender ! OperationAck(id)

    case Remove(key, id) =>
      kv = kv - key
      sender ! OperationAck(id)

    case Get(key, id) =>
      val result = kv.get(key)
      sender ! GetResult(key, result, id)

  }

  /* TODO Behavior for the replica role. */
  def replica(seq: Long): Receive = {

    case Get(key, id) =>
      val result = kv.get(key)
      sender ! GetResult(key, result, id)

    // incoming Snapshot request from Replicator
    case Snapshot(key, valueOption, incSeq) =>
      if (incSeq < seq ) sender ! SnapshotAck(key, incSeq)
      if (incSeq == seq) {

        valueOption match {
          // Case 1: Snapshot of an insert
          case Some(x) =>
            // Update the state of the secondary KV
            kv = kv.updated(key, x)

            // send a Persist request to the local persistence actor
            val id = Random.nextLong()

            // ask for a response from persistence. This should work
            // if persistence isn't flaky, but I'm not sure otherwise
            implicit val timeout = Timeout(100.millis)
            val replicator = sender

            ( persistence ? Persist(key, valueOption, id) )
              .onComplete {

                case Success(msg) =>
                  // send back the acknowledgement and update seq
                  println("trying to send Snapshot Ack... ")
                  println("Result: " + msg)
                  replicator ! SnapshotAck(key, incSeq)
                  context become replica(seq + 1)

                case Failure(t) =>
                  println(t)
                  println("Persistence failed. Trying again...")
                  self ! Snapshot(key, valueOption, incSeq)
              }


          case None =>

            kv = kv - key

            val id = Random.nextLong()

            implicit val timeout = Timeout(100.millis)
            val replicator = sender
            ( persistence ? Persist(key, valueOption, Random.nextLong()))
              .onSuccess { case _ =>
              replicator ! SnapshotAck(key, incSeq)

            }
            context become replica(seq + 1)
        }
      }

  }
}

