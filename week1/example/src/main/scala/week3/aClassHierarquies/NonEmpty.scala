package week3.aClassHierarquies

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean = {
    if (x < elem) {
      left.contains(x)
    } else if (x > elem) {
      right.contains(x)
    } else {
      true
    }
  }

  override def include(x: Int): IntSet = {
    if (x < elem) {
      new NonEmpty(elem, left.include(x), right)
    } else if (x > elem) {
      new NonEmpty(elem, left, right.include(x))
    } else {
      this
    }
  }

  override def union(other: IntSet): IntSet = {
    left.union(right)
      .union(other)
      .include(elem)
  }

  override def toString: String = {
    "{" + left + elem + right + "}"
  }
}
