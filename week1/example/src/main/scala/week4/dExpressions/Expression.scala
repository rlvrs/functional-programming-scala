package week4.dExpressions

trait Expr {
  def eval: Int = this match {
    case Number(n) => {
      n
    } case Sum(left, right) => {
      left.eval + right.eval
    } case Prod(left, right) => {
      left.eval * right.eval
    } case Var(name) => {
      // TODO: I think this should no longer return an Int but rather a String.
      ???
    }
  }

  def show: String = this match {
    case Number(n) => {
      n.toString
    } case Sum(left, right) => {
      s"${left.show} + ${right.show}"
    } case Prod(sumLeft: Sum, sumRight: Sum) => {
      s"${sumLeft.show} * (${sumRight.show})"
    } case Prod(left, sumRight: Sum) => {
      s"${left.show} * (${sumRight.show})"
    } case Prod(sumLeft: Sum, right) => {
      s"(${sumLeft.show}) * ${right.show}"
    } case Prod(left, right) => {
      s"${left.show} * ${right.show}"
    } case Var(name) => {
      name
    }
  }
}

case class Number(n: Int) extends Expr
case class Var(name: String) extends Expr
case class Sum(left: Expr, right: Expr) extends Expr
case class Prod(left: Expr, right: Expr) extends Expr
