package week2.cFunctionsAndData

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = {
    this(x, 1)
  }

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }
  }

  val numerator: Int = x / gcd(Math.abs(x), Math.abs(y))
  val denominator: Int = y / gcd(Math.abs(x), Math.abs(y))

  def + (that: Rational): Rational = {
    val newNumerator = numerator * that.denominator + that.numerator * denominator
    val newDenominator = denominator * that.denominator

    new Rational(newNumerator, newDenominator)
  }

  def unary_- (): Rational = {
    val newNumerator = -numerator
    new Rational(newNumerator, denominator)
  }

  def - (that: Rational): Rational = {
    this + -that
  }

  def < (that: Rational): Boolean = {
    numerator * that.denominator < that.numerator * denominator
  }

  def max(that: Rational): Rational = {
    if (this < that) {
      that
    } else {
      this
    }
  }

  override def toString: String = {
    "%s/%s".format(numerator, denominator)
  }

  def canEqual(a: Any): Boolean = a.isInstanceOf[Rational]

  override def equals(that: Any): Boolean = {
    that match {
      case that: Rational => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + numerator
    result = prime * result + denominator

    result
  }
}
