package week5.cHigherOrderFunctions

import org.scalatest.{FlatSpec, Matchers}

class TestHigherOrderFunctions extends FlatSpec with Matchers {
  val input = List("a", "a", "a", "b", "c", "c", "a")
  val outputPack = List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
  val outputEncode = List(("a", 3), ("b", 1), ("c", 2), ("a", 1))

  "A list" should "pack correctly" in {
    ListHigherOrderFuns.pack(input) should be(outputPack)
  }

  it should "encode correctly" in {
    ListHigherOrderFuns.encode(input) should be(outputEncode)
  }
}
