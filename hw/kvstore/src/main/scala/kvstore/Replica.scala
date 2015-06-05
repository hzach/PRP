package kvstore

import akka.actor._
import kvstore.Arbiter._
import kvstore.Persistence.{Persisted, Persist}
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration._
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

object Delegate {

  case class Do(seq: Long)
  case class Done(key:  String, id: Long)
  def props(persistor : ActorRef, replicator: ActorRef, req: Persist): Props =
    Props(new Delegate(persistor, replicator, req))

}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import Delegate._

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  var persistence = context.actorOf(persistenceProps)
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
    // overwritten if it if present
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

  var pending = List()

  /* TODO Behavior for the replica role. */
  def replica(seq: Long): Receive = {

    case Get(key, id) =>
      val result = kv.get(key)
      sender ! GetResult(key, result, id)

    // incoming Snapshot request from Replicator
    case Snapshot(key, valueOption, incSeq) =>

      // check if the seq is valid
      if (incSeq < seq ) sender ! SnapshotAck(key, incSeq)
      if (incSeq == seq) {

        // update the store
        valueOption match {

          // Snapshot of an insert
          case Some(x) => kv = kv.updated(key, x)

          // Snapshot of a removal
          case None => kv = kv - key

        }

        // send a Persist request to the local persistence actor
        val req = Persist(key, valueOption, incSeq)
        val delegate = context.actorOf(Delegate.props(persistence, sender(), req))
        delegate ! Do

        // finished! the delegate will do the work for us
        // update the seq of this
        context become replica(seq + 1)
      }
  }
}

/**
 * Delegate actor spawned by a replica to help keep track of the actor refs to the persistor and
 * replicator while it sends requests to the persistor. Once started, it will request persistence
 * of an update from the persistor and will retry this request until the persistor confirms that
 * the request was satisfied.
 * /*TODO: This can be improved by setting and upper limit on the number of times the request
 *  *TODO: may be sent. Will need to use DeathWatch in the parent to make this work.
 *  */
 * @param persistor actor ref to the local persistence actor
 * @param replicator actor ref to this parent's corresponding replicator
 */
class Delegate(val persistor: ActorRef, replicator: ActorRef, req: Persist) extends Actor {
  import Replicator._
  import Persistence._
  import Delegate._

  implicit val timeout = Timeout(501.millis)

  def receive: Receive = {

    case Do =>
      persistor ! req
      context.setReceiveTimeout(100.millis)

    case ReceiveTimeout =>
      self ! Do

    case Persisted(key, id) =>
      replicator ! SnapshotAck(key, id)
      self ! PoisonPill

  }
}

