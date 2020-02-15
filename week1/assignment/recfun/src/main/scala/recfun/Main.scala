package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def getBalanceWeightIncrement(c: Char): Int = {
      c match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
    }

    def balanceIter(chars: List[Char], balanceWeight: Int): Boolean = {
      if (chars.isEmpty) {
        balanceWeight == 0
      } else if (balanceWeight < 0) {
        false
      } else {
        val newBalanceWeight = balanceWeight + getBalanceWeightIncrement(chars.head)
        balanceIter(chars.tail, newBalanceWeight)
      }
    }

    balanceIter(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) {
      1
    } else if(money < 0 || coins.isEmpty) {
      0
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
