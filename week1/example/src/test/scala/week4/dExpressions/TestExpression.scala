package week4.dExpressions

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestExpression extends FlatSpec with Matchers {
  // 2 * x + y
  val twoTimesXPlusY: Expr = Sum(Prod(Number(2), Var("x")), Var("y"))
  // (2 + x) * y
  val twoPlusXTimesY: Expr = Prod(Sum(Number(2), Var("x")), Var("y"))

  "An ordered by precedence expression" should "return a string without parentheses" in {
    twoTimesXPlusY.show should be("2 * x + y")
  }

  "A non ordered by precedence expression" should "return a string with parentheses" in {
    twoPlusXTimesY.show should be("(2 + x) * y")
  }
}
