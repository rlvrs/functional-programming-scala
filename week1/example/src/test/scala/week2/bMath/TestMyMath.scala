package week2.bMath

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestMyMath extends FunSuite{
  import MyMath._

  test("sqrt of 2 should be around 1.4142") {
    val sqrt2 = sqrt(2)
    assert(sqrt2 > 1.4141 && sqrt2 < 1.4143)
  }
}
