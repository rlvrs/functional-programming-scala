package week5.aPairsAndTuples

import org.scalatest.{FlatSpec, Matchers}
import week5.bImplicits.MergeSortImplicitObj

class TestMergeSortObj extends FlatSpec with Matchers {
  val list0 = List(4,2,3,1)
  val list1 = List(1,4)
  val list2 = List(2,3)
  val expectedMerge1_2 = List(1,2,3,4)
  val fruits_4 = List("bananas", "apples", "oranges", "pears")
  val fruits_4_sorted = List("apples", "bananas", "oranges", "pears")

  "Two lists" should "be orderly merged" in {
    MergeSortObj.merge(list1, list2)(Ordering.Int) should be(expectedMerge1_2)
  }

  "An unordered" should "be correctly sorted" in {
    MergeSortObj.msort(list0)(Ordering.Int) should be(expectedMerge1_2)
    MergeSortObj.msort(fruits_4)(Ordering.String) should be(fruits_4_sorted)
  }

  "An unordered" should "be correctly sorted using implicits" in {
    MergeSortImplicitObj.msort(list0) should be(expectedMerge1_2)
    MergeSortImplicitObj.msort(fruits_4) should be(fruits_4_sorted)
  }
}
