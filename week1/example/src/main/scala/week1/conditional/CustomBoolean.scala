package week1.conditional

object CustomBoolean {
  /**
    * Submitted solution:
    * def and(x:Boolean,y: =>Boolean) = if(x) y else false
    *
    * @param x left operand of and
    * @param y right operand of and
    * @return the result of the and operation
    */
  def and(x: Boolean, y: => Boolean): Boolean = {
    if (x) {
      y
    } else {
      false
    }
  }

  /**
    * Submitted solution:
    * def or(x:Boolean,y: =>Boolean) = if(!x) y else true
    *
    * @param x left operand of or
    * @param y right operand of or
    * @return the result of the or operation
    */
  def or(x: Boolean, y: => Boolean): Boolean = {
    if (!x) {
      y
    } else {
      true
    }
  }
}
