package week4.cCovarianceLists

object PureNil extends PureList[Nothing] {
  override def isEmpty: Boolean = {
    true
  }

  override def head: Nothing = {
    throw new NoSuchElementException("Nil.head")
  }

  override def tail: Nothing = {
    throw new NoSuchElementException("Nil.tail")
  }
}
