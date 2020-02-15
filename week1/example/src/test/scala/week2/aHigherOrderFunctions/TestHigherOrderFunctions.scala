package week2.aHigherOrderFunctions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestHigherOrderFunctions extends FunSuite{
  import HigherOrderFunctions._

  test("Sum of identity 1 to 4 should return 10.") {
    assert(sum(x => x)(1, 4) === 10)
  }

  test("Sum of squares 1 to 4 should return 30.") {
    assert(sum(x => x*x)(1, 4) === 30)
  }

  test("Product of identity 1 to 4 should return 24.") {
    assert(product(x => x)(1, 4) === 24)
  }

  test("Product of Squares 1 to 4 should return 576.") {
    assert(product(x => x*x)(1, 4) === 576)
  }

  test("Factorial 4 should return 24.") {
    assert(factorial(4) === 24)
  }
}
