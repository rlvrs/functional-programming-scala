package week4.aPureBoolean

object PureFalse extends PureBoolean {
  override def ifThenElse[T](then_expression: => T, else_expression: => T): T = {
    else_expression
  }
}
