package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for{
      k <- arbitrary[A]
      //m <- oneOf(empty,genHeap)
      m <- genHeap
    } yield  insert(k,m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2Elements") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a,b)
  }

  property("insertDelete") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorted") = forAll { (heap: H) =>
    def isSorted (heap:H): Boolean = {
      def createList(heap: H, acc: List[A]): List[A] = heap match {
        case min :: Nil => findMin(heap) :: acc
        case min :: tail => createList(deleteMin(heap), findMin(heap) :: acc)
      }
      def checkSorted(list: List[A]):Boolean = list match {
        case Nil => true
        case head :: Nil => true
        case head :: tail => {
          if (head > tail.head) checkSorted(tail)
          else false
        }
      }
      checkSorted(createList(heap, List()))
    }
    isSorted(heap)
  }

  property("melded minimum") = forAll{ (heap1: H, heap2: H)=>
    val minMelded = findMin(meld(heap1, heap2))
    minMelded == findMin(heap1) || minMelded == findMin(heap2)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
