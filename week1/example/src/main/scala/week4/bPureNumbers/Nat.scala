package week4.bPureNumbers

/**
  * This class represents all non-negative integers.
  * It is called Peano numbers.
  */
abstract class Nat {
  /**
    * @return True if the natural number i zero. False, otherwise.
    */
  def isZero: Boolean

  /**
    * @throws java.lang.Exception if the natural number is zero.
    * @return the previous natural number.
    */
  @throws(classOf[Exception])
  def predecessor: Nat

  /**
    * @return the next natural number
    */
  def successor: Nat = {
    new Succ(this)
  }

  /**
    * Addition operation on natural numbers.
    *
    * @param that
    * @return
    */
  def + (that: Nat): Nat

  /**
    * Subtraction operation on natural numbers
    *
    * @param that
    * @throws java.lang.Exception if the result is negative.
    * @return
    */
  @throws(classOf[Exception])
  def - (that: Nat): Nat

  def countsToZero(): Int

  def == (that: Nat): Boolean = {
    this.countsToZero() == that.countsToZero()
  }
}
