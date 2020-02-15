package week4.aPureBoolean

abstract class PureBoolean {
  def ifThenElse[T](then_expression: => T, else_expression: => T): T

  def && (x: => Boolean): Boolean = {
    this.ifThenElse[Boolean](x, false)
  }

  def || (x: => Boolean): Boolean = {
    this.ifThenElse[Boolean](true, x)
  }

  def unary_! : Boolean = {
    this.ifThenElse[Boolean](false, true)
  }

  def == (x: Boolean): Boolean = {
    this.ifThenElse[Boolean](x, x.unary_!)
  }

  def != (x: Boolean): Boolean = {
    this.ifThenElse[Boolean](x.unary_!, x)
  }

  /**
    * This method assumes that "false < true" as instructed during classes for learning purposes.
    *
    * @param x
    * @return
    */
  def < (x: Boolean): Boolean = {
    this.ifThenElse(false, x)
  }
}
