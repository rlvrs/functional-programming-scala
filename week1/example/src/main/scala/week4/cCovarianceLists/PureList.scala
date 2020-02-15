package week4.cCovarianceLists

import week3.aClassHierarquies.{Empty, IntSet, NonEmpty}

/**
  * Some types should be covariant whereas others should not.
  *
  * Lists should be covariant and
  * Arrays should not be covariant. The following snippet shows why.
  *
  * Roughly speaking, a type that accepts mutations of its elements
  * should be covariant.
  *
  * But immutable types can be covariant, if some conditions on methods are met.
  *
  * NonEmpty [] a = new  NonEmpty []{ new  NonEmpty (1, Empty , Empty )}
  *
  * // Scala has a type error here (at compile time), since arrays are not covariant
  * IntSet [] b = a
  *
  * // Java throws ArrayStoreException here, since every array should have a single type associated
  * b[0] = Empty
  * NonEmpty s = a[0]
  *
  */
trait PureList[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: PureList[T]

  /**
    * Covariant types ([+T], which makes it possible to accept PureNil[Nothing].
    *                 Nothing is a subtype of every object.)
    *
    * Covariant type parameters may appear in lower bounds of method type parameters.
    * Contravariant type parameters may appear in upper bounds of method.
    *
    * @param elem
    * @tparam U
    * @return
    */
  def prepend[U >: T](elem: U): PureList[U] = {
    Empty
    PureCons(elem, this)
  }

  def last(): T = {
    def loop(xs: PureList[T]): T = {
      xs match {
        case PureNil => {
          throw new NoSuchElementException("Nil.last")
        } case PureCons(head, PureNil) => {
          head
        } case PureCons(head, tail) => {
          loop(tail)
        }
      }
    }

    loop(this)
  }

  def init(): PureList[T] = {
    def loop(xs: PureList[T]): PureList[T] = {
      xs match {
        case PureNil => {
          throw new NoSuchElementException("Nil.init")
        } case PureCons(head, PureNil) => {
          PureNil
        } case PureCons(head, tail) => {
          loop(xs.tail).prepend(head)
        }
      }
    }

    loop(this)
  }

  def concat[U >: T](otherList: PureList[U]): PureList[U] = {
    def loop(xs: PureList[U], ys: PureList[U]): PureList[U] = {
      xs match {
        case PureNil => {
          ys
        } case PureCons(head, tail) => {
          loop(xs.tail, ys).prepend(xs.head)
        }
      }
    }

    loop(this, otherList)
  }

  // Complexity is n*n. "++" leads to "n" and xs leads to "n" as well
  def reverse(): PureList[T] = {
    def loop(xs: PureList[T]): PureList[T] = {
      xs match {
        case PureNil => {
          PureNil
        } case PureCons(head, tail) => {
          loop(xs.tail).concat(PureList(xs.head))
        }
      }
    }

    loop(this)
  }

  /**
    * The map defined in the standard scala library is implemented
    * using tail recursion and works for arbitrary collections. Not just lists.
    */
  def map[U >: T](fun: T => U): PureList[U] = {
    this match {
      case PureNil => {
        this
      } case PureCons(head, tail) => {
        tail.map(fun).prepend(fun(head))
      }
    }
  }

  /*
  TODO: implement these
   */
  def removeAt[T](n: Int, xs: PureList[T]) = {
    //(xs take n) ::: (xs drop n+1)
    ???
  }

  def flatten(xs: List[Any]): List[Any] = {
/*
    def loop(xs: List[Any]): List[Any] = {

    }

    loop()
    */
    ???
  }


}

object PureList {
  def apply[T](elems: T*): PureList[T] = {
    def loop(elems: Seq[T], acc: PureList[T]): PureList[T] = {
      if (elems.isEmpty) {
        acc
      } else {
        loop(elems.tail, acc.prepend(elems.head))
      }
    }

    loop(elems.reverse, PureList())
  }
/*
  // PureList(1, 2, 3, 4) = PureList.apply(1, 2, 3, 4)
  def apply[T](x1: T, x2: T, x3: T, x4: T): PureList[T] = {
    new PureCons[T](x1, PureList(x2, x3, x4))
  }

  // PureList(1, 2, 3) = PureList.apply(1, 2, 3)
  def apply[T](x1: T, x2: T, x3: T): PureList[T] = {
    new PureCons[T](x1, PureList(x2, x3))
  }

  // PureList(1, 2) = PureList.apply(1, 2)
  def apply[T](x1: T, x2: T): PureList[T] = {
    new PureCons[T](x1, PureList(x2))
  }

  // PureList(1) = PureList.apply(1)
  def apply[T](x: T): PureList[T] = {
    new PureCons[T](x, PureNil)
  }
*/
  // PureList() = PureList.apply()
  def apply[T](): PureList[T] = {
    PureNil
  }

  def squareList1(xs: PureList[Int]): PureList[Int] = {
    xs match {
      case PureNil => {
        xs
      } case PureCons(y, ys) => {
        squareList1(ys).prepend(y*y)
      }
    }
  }

  def squareList(xs: PureList[Int]): PureList[Int] = {
    xs map (x => x*x)
  }
}

object test extends App {
  /**
    * This method receives
    *
    * @param xs
    * @param x
    * @return
    */
  def f(xs: PureList[NonEmpty], x: Empty.type): PureList[IntSet] = {
    xs prepend x
  }

  def main() = {
    PureList("a", "b", "c", "d")
  }

  main()
}
