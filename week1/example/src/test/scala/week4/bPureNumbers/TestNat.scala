package week4.bPureNumbers

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestNat extends FlatSpec with Matchers {
  val nat1 = new Succ(Zero)
  val nat2 = new Succ(nat1)
  val nat3 = new Succ(nat2)

  "A Zero" should "be zero" in {
    Zero.isZero should be(true)
  }

  "A Succ" should "not be zero" in {
    nat1.isZero should be(false)
  }

  "A Zero" should "throw Exception when requesting its predecessor" in {
    a[Exception] should be thrownBy {
      Zero.predecessor
    }
  }

  "A Succ" should "have its predecessor eq to its field" in {
    nat1.predecessor should be (Zero)
  }

  "The sucessor of a natural 1" should "be natural2" in {
    (nat1.successor == nat2) should be (true)
  }

  "The sucessor of Zero" should "be natural1" in {
    (Zero.successor == nat1) should be (true)
  }

  "Two natural numbers" should "add" in {
    (Zero + nat1) == nat1 should be (true)
    (nat2 + nat1) == nat3 should be (true)
  }

  "Addition" should "be commutative" in {
    (nat2 + nat1) == nat3 should be (true)
    (nat1 + nat2) == nat3 should be (true)
  }

  "A Zero" should "not be subtracted" in {
    a[Exception] should be thrownBy {
      Zero - nat1
    }
  }

  "A subtraction" should "not return less than Zero" in {
    a[Exception] should be thrownBy {
      nat1 - nat2
      nat1 - nat3
      nat2 - nat3
      Zero - nat1
      Zero - nat2
      Zero - nat3
    }
  }

  "A subtraction" should "be work for naturals" in {
    (Zero - Zero) == Zero should be (true)
    (nat3 - nat1) == nat2 should be (true)
    (nat3 - nat2) == nat1 should be (true)
  }
}
