package week4.aPureBoolean

object PureTrue extends PureBoolean {
  override def ifThenElse[T](then_expression: => T, else_expression: => T): T = {
    then_expression
  }
}
