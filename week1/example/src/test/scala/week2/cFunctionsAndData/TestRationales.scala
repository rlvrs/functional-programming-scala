package week2.cFunctionsAndData

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestRationales extends FlatSpec with Matchers {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  "Adding x and z" should "return 11/6" in {
    x + z should be(new Rational(11, 6))
  }

  "Subtracting y and z to x" should "return 79/42" in {
    val xMinusyz: Rational = x - y - z
    val expected: Rational = new Rational(-79, 42)
    xMinusyz should be(expected)
  }

  "Adding y and y" should "return 10/7" in {
    y + y should be(new Rational(10, 7))
  }

  "x < y" should "be true" in {
    x < y should be(true)
  }

  "max(x,y)" should "be y" in {
    x.max(y) should be(y)
  }

  "A Rationale" should "throw IllegalArgumentException if the denominator is zero" in {
    a[IllegalArgumentException] should be thrownBy {
      new Rational(1, 0)
    }
  }
}
