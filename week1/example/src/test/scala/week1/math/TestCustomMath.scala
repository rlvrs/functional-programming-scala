package week1.math

import org.scalatest.FunSuite
import CustomMath._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestCustomMath extends FunSuite {
  // Square root

  test("sqrt of 2 should be around 1.4142") {
    val sqrt2 = sqrt(2)
    assert(sqrt2 > 1.4141 && sqrt2 < 1.4143)
  }

  test("sqrt of 4 should be around 2") {
    val sqrt4 = sqrt(4)
    assert(sqrt4 > 1.999 && sqrt4 < 2.001)
  }

  test("sqrt of 1e-6 should be around 1e-3") {
    val sqrt1eMinus6 = sqrt(1e-6)
    assert(sqrt1eMinus6 > 1e-4 && sqrt1eMinus6 < 1e-2)
  }

  test("sqrt of 1e60 should be around 1e30") {
    val sqrt1e60 = sqrt(1e60)
    assert(sqrt1e60 > 1e29 && sqrt1e60 < 1e31)
  }

  // Absolute

  test("Absolute of negative should return positive") {
    assert(abs(-2) === 2)
  }

  test("Absolute of positive should return itself") {
    assert(abs(2) === 2)
  }

  // Factorial Tail Rec
  test("Factorial of 0 should return 1") {
    assert(factorialTailRec(0) === 1)
  }

  test("Factorial of 1 should return 1") {
    assert(factorialTailRec(1) === 1)
  }

  test("Factorial of 2 should return 2") {
    assert(factorialTailRec(2) === 2)
  }

  test("Factorial of 3 should return 6") {
    assert(factorialTailRec(3) === 6)
  }

  test("Factorial of 4 should return 24") {
    assert(factorialTailRec(4) === 24)
  }

  test("Factorial of 5 should return 120") {
    assert(factorialTailRec(5) === 120)
  }
}
