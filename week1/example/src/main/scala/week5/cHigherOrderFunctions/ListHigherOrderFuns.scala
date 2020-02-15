package week5.cHigherOrderFunctions

object ListHigherOrderFuns {
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  nums filter (x => x > 0) // List(2, 5, 7, 1)
  nums filterNot (x => x > 0) // List(-4)
  nums partition (x => x > 0) // List(List(-4), List(2, 5, 7, 1))

  nums takeWhile (x => x > 0) // List(2)
  nums dropWhile (x => x > 0) // List(-4, 5, 7, 1)
  nums span (x => x > 0) // List(List(2), List(-4, 5, 7, 1))

  def pack[T](xs: List[T]): List[List[T]] = {
    xs match {
      case Nil => {
        Nil
      } case x :: xs1 => {
        val (first, rest) = xs.span(elem => elem == x)
        first :: pack(rest)
      }
    }
  }

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map (l => (l.head, l.length))
  }

  /*
  TODO: implement these in a List class

  def reduceLeft(op: (T, T) => T): T = {
    this match {
      case Nil => {
        throw new Error("Nil.reduceLeft")
      }
      case x :: xs => {
        (xs foldLeft x) (op)
      }
    }
  }

  def foldLeft[U](z: U)(op:(U, T) => U): U = {
    this match {
      case Nil => {
        z
      } case x :: xs => {
        (xs foldLeft op(z, x))(op)
      }
    }
  }
*/
/*
  def reduceRight(op: (T, T) => T): T = {
    this match {
      case Nil => {
        throw new Error("Nil.reduceRight")
      } case x :: Nil => {
        x
      } case x :: xs => {
        op(x, xs.  reduceRight(op))
      }
    }
  }

  def foldRight[U](z: U)(op:(T, U) => U): U = {
    this match {
      case Nil => {
        z
      } case x :: xs => {
        op(x, (xs foldRight z)(op))
      }
    }
  }

  // TODO: concat function with foldRight. Check where to put it.
  def concatAlternative()

  def mapFun[T, U](xs: List[T], f: T => U): List[U] = {
    (xs foldRight List[U]())( ??? )
  }

  def lengthFun[T](xs: List[T]): Int = {
    (xs foldRight 0)( ??? )
  }
*/
}
