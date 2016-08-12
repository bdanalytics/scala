package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

// Added imports
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)
//  } yield insert(e, empty)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("hint1") = forAll {
//    (e1: Int, e2: Int) =>
    (e1: A, e2: A) =>
      val h = insert(e2, insert(e1, empty))
      findMin(h) == min(e1, e2)
  }

  property("hint2") = forAll {
    e: A =>
      val h = insert(e, empty)
      isEmpty(deleteMin(h))
  }

  property("hint3") = forAll {
    h: H =>
      def isSorted(h: H): Boolean =
        if (isEmpty(h)) true
        else {
          val me = findMin(h)
          val rh = deleteMin(h)
          isEmpty(rh) || ((me <= findMin(rh)) && isSorted(rh))
        }

      isSorted(h)
  }

  property("hint4") = forAll {
    (h1: H, h2: H) =>
      findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("meld equivalent") = forAll {
    (h1: H, h2: H) =>
      def heapEqual(h1: H, h2: H): Boolean =
        if (isEmpty(h1) && isEmpty(h2)) true
        else {
          val m1 = findMin(h1)
          val m2 = findMin(h2)
          (m1 == m2) && heapEqual(deleteMin(h1), deleteMin(h2))
        }

      heapEqual(meld(h1, h2),
                meld(deleteMin(h1), insert(findMin(h1), h2)))
      findMin(meld(deleteMin(h1), insert(findMin(h1), h2))) == min(findMin(h1), findMin(h2))
  }

//  property("display heap") = forAll {
//    h: H =>
//      def getSeq(l: List[A], h: H): List[A] = isEmpty(h) match {
//        case true => l.reverse
//        case false => getSeq(findMin(h) :: l, deleteMin(h))
//      }
//
////    println("thsHeap: " + getSeq(List(), h))
//  }

  property("meld two heaps with mins") = forAll { (h1: H, h2: H) =>
    def getSeq(l: List[A], heap: H): List[A] = isEmpty(heap) match {
      case true => l.reverse
      case false => getSeq(findMin(heap) :: l, deleteMin(heap))
    }

    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val melded = getSeq(List(), meld(h1, h2))
      val melded2 = getSeq(List(), meld(deleteMin(h1), insert(findMin(h1), h2)))
      melded.sorted == melded2.sorted
    }
  }
}
