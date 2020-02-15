package week3.aClassHierarquies

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class TestIntSet extends FlatSpec with Matchers {
  val tree1 = new NonEmpty(3, Empty, Empty)
  val tree2 = tree1.include(4)

  "A tree" should "be have toString" in {
    tree1.toString should be("{.3.}")
    tree2.toString should be("{.3{.4.}}")
  }

  "A tree" should "union with other" in {

  }
}
