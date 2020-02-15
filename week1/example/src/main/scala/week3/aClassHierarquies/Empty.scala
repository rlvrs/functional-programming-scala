package week3.aClassHierarquies

object Empty extends IntSet {
  override def contains(x: Int): Boolean = {
    false
  }

  override def include(x: Int): IntSet = {
    new NonEmpty(x, Empty, Empty)
  }

  override def union(other: IntSet): IntSet = {
    other
  }

  override def toString: String = {
    "."
  }
}
