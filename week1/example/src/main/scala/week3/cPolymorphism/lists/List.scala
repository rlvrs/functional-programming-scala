package week3.cPolymorphism.lists

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

object List {
  // List(1, 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil[T]))

  // List(1) = List.apply(1)
  def apply[T](x: T): List[T] = new Cons[T](x, new Nil[T])

  // List() = List.apply()
  def apply[T](): List[T] = new Nil[T]
}
