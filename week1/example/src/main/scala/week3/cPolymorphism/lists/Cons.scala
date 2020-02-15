package week3.cPolymorphism.lists

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = {
    false
  }

  def singleton(element: T): List[T] = {
    new Cons[T](element, new Nil[T])
  }

  def nth(n: Int): T = {
    def loop(i: Int, list: List[T]): T = {
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
