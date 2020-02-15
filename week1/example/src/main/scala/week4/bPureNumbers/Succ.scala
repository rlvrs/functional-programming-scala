package week4.bPureNumbers

class Succ(n: Nat) extends Nat {
  /**
    * @return True if the natural number i zero. False, otherwise.
    */
  override def isZero: Boolean = {
    false
  }

  /**
    * @throws java.lang.Exception if the natural number is zero.
    * @return the previous natural number.
    */
  override def predecessor: Nat = {
    n
  }

  /**
    * Addition operation on natural numbers.
    * Non tailrec solution: new Succ(n + that)
    *
    * @param that
    * @return
    */
  override def +(that: Nat): Nat = {
    def loop(iter: Nat, acc: Nat): Nat = {
      if (iter.isZero) {
        acc
      } else {
        loop(iter.predecessor, acc.successor)
      }
    }

    loop(that, this)
  }

  /**
    * Subtraction operation on natural numbers.
    * Non tailrec solution: if (that.isZero) this else n - that.predecessor
    *
    * @param that
    * @throws java.lang.Exception if the result is negative.
    * @return
    */
  override def -(that: Nat): Nat = {
    def loop(iter: Nat, acc: Nat): Nat = {
      if (iter.isZero) {
        acc
      } else {
        loop(iter.predecessor, acc.predecessor)
      }
    }

    loop(that, this)
  }

  override def countsToZero(): Int = {
    def loop(nat: Nat, acc: Int): Int = {
      if (nat.isZero) {
        acc
      } else {
        loop(nat.predecessor, acc+1)
      }
    }

    loop(this, 0)
  }
}
