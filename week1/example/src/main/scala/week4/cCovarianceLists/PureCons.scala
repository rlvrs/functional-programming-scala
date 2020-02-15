package week4.cCovarianceLists

case class PureCons[T](head: T, tail: PureList[T]) extends PureList[T] {
  override def isEmpty: Boolean = {
    false
  }

  def singleton(element: T): PureList[T] = {
    new PureCons[T](element, PureNil)
  }

  def nth(n: Int): T = {
    def loop(i: Int, list: PureList[T]): T = {
      if (list.isEmpty) {
        throw new IndexOutOfBoundsException(s"The index $n does not exist.")
      } else if (i == n) {
        list.head
      } else {
        loop(i+1, list.tail)
      }
    }
    loop(0, this)
  }
}
