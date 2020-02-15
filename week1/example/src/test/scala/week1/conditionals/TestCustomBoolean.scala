package week1.conditionals

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import week1.conditional.CustomBoolean._

@RunWith(classOf[JUnitRunner])
class TestCustomBoolean extends FunSuite {
  // Test AND.
  test("true and true should return true") {
    assert(and(true, true) === true)
  }

  test("false and true should return false") {
    assert(and(false, true) === false)
  }

  test("true and false should return false") {
    assert(and(true, false) === false)
  }

  test("false and false should return false") {
    assert(and(false, false) === false)
  }

  // Test OR.
  test("true or true should return true") {
    assert(or(true, true) === true)
  }

  test("false or true should return true") {
    assert(or(false, true) === true)
  }

  test("true or false should return true") {
    assert(or(true, false) === true)
  }

  test("false or false should return false") {
    assert(or(false, false) === false)
  }
}
