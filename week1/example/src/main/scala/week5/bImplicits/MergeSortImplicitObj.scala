package week5.bImplicits

object MergeSortImplicitObj {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (xs.isEmpty || xs.length == 1) {
      xs
    } else {
      val (fst, snd) = xs.splitAt(n)
      merge(msort(fst), msort(snd))(ord)
    }
  }

  def merge[T](xs: List[T], ys: List[T])(implicit ord: Ordering[T]): List[T] = {
    (xs, ys) match {
      case (Nil, `ys`) => {
        ys
      } case (`xs`, Nil) => {
        xs
      } case (x :: xs1, y :: ys1) => {
        if (ord.lt(x, y)) {
          x :: merge(xs1, ys)
        } else {
          y :: merge(xs, ys1)
        }
      }
    }
  }
}
