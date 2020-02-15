package week3.aClassHierarquies

abstract class IntSet {
  def include(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(that: IntSet): IntSet
}
