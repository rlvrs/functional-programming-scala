package week4.cPureLists

import org.scalatest.{FlatSpec, Matchers}
import week4.cCovarianceLists.PureList

class TestPureList extends FlatSpec with Matchers {
  val fruits_3_reversed = PureList[String]("pears", "oranges", "apples")
  val fruits_3 = PureList[String]("apples", "oranges", "pears")
  val fruits_4 = PureList[String]("banana", "apples", "oranges", "pears")
  val emptyList = PureList()
  val numbers = PureList[Int](4, 9)
  val numbers_scaled2 = PureList[Int](8, 18)
  val numbers_squared2 = PureList[Int](16, 81)

  "A Purelist" should "return its correct last element" in {
    fruits_3.last() should be("pears")
  }

  it should "return its correct head element" in {
    fruits_3.head should be("apples")
  }

  it should "throw NoSuchElementException when list is empty" in {
    a[NoSuchElementException] should be thrownBy {
      emptyList.last()
      emptyList.head
      emptyList.init()
    }
  }

  it should "prepend an element" in {
    fruits_3.prepend("banana").head should be("banana")
    fruits_3.prepend("banana") should be(PureList("banana", "apples", "oranges", "pears"))
  }

  it should "return the correct init" in {
    fruits_3.init() should be(PureList("apples", "oranges"))
  }

  it should "concatenate lists correctly" in {
    emptyList.concat(fruits_3) should be(fruits_3)
    fruits_3.concat(emptyList) should be(fruits_3)
    PureList("banana").concat(fruits_3) should be(fruits_4)
  }

  it should "reverse correctly" in {
    fruits_3.reverse() should be(fruits_3_reversed)
  }

  it should "map scaling by 2" in {
    numbers.map(x => x*2) should be(numbers_scaled2)
  }

  it should "map power of 2" in {
    PureList.squareList(numbers) should be(numbers_squared2)
    PureList.squareList1(numbers) should be(numbers_squared2)
  }

  // removeAt(1, List('a', 'b', 'c', 'd'))
  // >   res0: List[Any] = List(a, c, d)


  // flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  // >   res0: List[Any] = List(1, 1, 2, 3, 5, 8)
}
