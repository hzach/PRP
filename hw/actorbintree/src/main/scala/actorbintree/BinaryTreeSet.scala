/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  def doOperation(newRef: ActorRef, op: Operation) = op match {

    case Insert(ref, id, elem) =>
      Insert(newRef, id, elem)
    case Contains(ref, id, elem) =>
      Contains(newRef, id, elem)
    case Remove(ref, id, elem) =>
      Remove(newRef, id, elem)

  }

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {

    case o: Operation => root ! o

    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context become garbageCollecting(newRoot)

  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {

    case o: Operation =>
      pendingQueue = pendingQueue.enqueue(o)
    case CopyFinished =>
      //kill the current root
      root ! PoisonPill

      //set the root to new root
      root = newRoot

      //send all pending operations to the root. Reset the operation queue
      pendingQueue foreach { root !  _ }
      pendingQueue = Queue.empty[Operation]

      //set the context back to normal
      context become normal

  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case i: Insert =>

      if (elem == i.elem) {
        removed = false
        i.requester ! OperationFinished(i.id)
      } else {
        val pos = if (elem < i.elem) Right else Left
        subtrees.get(pos) match {
          case Some(node) =>
            node ! Insert(i.requester, i.id, i.elem)
          case None =>
            subtrees += pos -> context.actorOf(props(i.elem, initiallyRemoved = false))
            i.requester ! OperationFinished(i.id)
        }


      }

    case c: Contains =>

      //check the right subtree
      if (elem == c.elem) {
        c.requester ! ContainsResult(c.id, !removed)
      } else {
        val pos = if (elem < c.elem) Right else Left
        subtrees.get(pos) match {
          case Some(node) => node ! Contains(c.requester, c.id, c.elem)
          case None => c.requester ! ContainsResult(c.id, false)
        }
      }


    case r: Remove =>

      if ( elem == r.elem) {
        removed = true
        r.requester ! OperationFinished(r.id)
      } else {
        val pos = if (elem < r.elem) Right else Left
        subtrees.get(pos) match {
          case Some(node) =>
            node ! Remove(r.requester, r.id, r.elem)
          case None =>
            r.requester ! OperationFinished(r.id)
        }
      }

    case CopyTo(treeNode) =>

      val expected = subtrees.values.toSet
      expected foreach{ _ ! CopyTo(treeNode) }
      context become copying(expected, removed)

      if (!removed) {
        treeNode ! Insert(self, 0, elem)
      }

      if (expected.isEmpty && removed) {
        context.parent ! CopyFinished
      }




  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) =>
      if (expected.isEmpty) context.parent ! CopyFinished
      else context become copying(expected, true)

    case CopyFinished =>
      val remaining = expected - sender
      if (remaining.isEmpty && insertConfirmed) context.parent ! CopyFinished
      else context become copying(remaining, insertConfirmed)
  }


}
