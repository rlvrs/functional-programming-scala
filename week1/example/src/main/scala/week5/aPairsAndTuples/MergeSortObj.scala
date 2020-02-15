package week5.aPairsAndTuples

object MergeSortObj {
  def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (xs.isEmpty || xs.length == 1) {
      xs
    } else {
      val (fst, snd) = xs.splitAt(n)
      merge(msort(fst)(ord), msort(snd)(ord))(ord)
    }
  }

  def merge[T](xs: List[T], ys: List[T])(ord: Ordering[T]): List[T] = {
    (xs, ys) match {
      case (Nil, `ys`) => {
        ys
      } case (`xs`, Nil) => {
        xs
      } case (x :: xs1, y :: ys1) => {
        if (ord.lt(x, y)) {
          x :: merge(xs1, ys)(ord)
        } else {
          y :: merge(xs, ys1)(ord)
        }
      }
    }
  }
}
