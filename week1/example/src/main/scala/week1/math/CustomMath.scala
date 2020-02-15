package week1.math

object CustomMath {
  def abs(x: Double): Double = {
    if (x < 0) {
      -x
    } else {
      x
    }
  }

  def sqrt(x: Double): Double = {

    def isGoodEnough(guess: Double): Boolean = {
      val absDiffOfGuess = abs(guess * guess - x)
      // Support really small numbers and really huge numbers.
      val absDiffOfGuessNormalized = absDiffOfGuess / x
      absDiffOfGuessNormalized < 0.001
    }

    def improve(guess: Double): Double = {
      val quotient = x / guess
      val mean = (guess + quotient) / 2
      mean
    }

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) {
        guess
      } else {
        sqrtIter(improve(guess))
      }
    }

    sqrtIter(1.0)
  }

  def factorialTailRec(n: Int): Int = {

    def factorialIter(currN: Int, accumulator: Int): Int = {
      if (currN == 0) {
        accumulator
      } else {
        factorialIter(currN - 1, accumulator * currN)
      }
    }

    factorialIter(n, 1)
  }
}
