package quickcheck

import common._

import quickcheck._
import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //Heap properties
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    findMin(insert(b, h)) == math.min(a, b)
  }

  property("Del1Empty") = forAll { a: Int =>
    val h = insert(a, empty)
    val h_rm = deleteMin(h)
    h_rm == empty
  }

  property("erase ")

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
