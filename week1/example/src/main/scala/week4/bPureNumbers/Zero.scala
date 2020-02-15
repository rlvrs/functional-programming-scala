package week4.bPureNumbers

object Zero extends Nat {
  /**
    * @return True if the natural number i zero. False, otherwise.
    */
  override def isZero: Boolean = {
    true
  }

  /**
    * @throws java.lang.Exception if the natural number is zero.
    * @return the previous natural number.
    */
  override def predecessor: Nat = {
    throw new Exception("The predecessor of zero is not a natural number.")
  }

  /**
    * Addition operation on natural numbers.
    *
    * @param that
    * @return
    */
  override def +(that: Nat): Nat = {
    that
  }

  /**
    * Subtraction operation on natural numbers
    *
    * @param that
    * @throws java.lang.Exception if the result is negative.
    * @return
    */
  override def -(that: Nat): Nat = {
    if (that.isZero) {
      this
    } else {
      throw new Exception("Cannot substract from zero.")
    }
  }

  override def countsToZero(): Int = {
    0
  }
}
