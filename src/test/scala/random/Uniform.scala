package random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import cats._
import cats.implicits._

class UniformSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks {
  def low_high(a: Int, b: Int): (Int, Int) =
    (scala.math.min(a, b), scala.math.max(a, b))

  "random uniform integer" should "fall into inclusive range" in {
    forAll { (a: Int, b: Int) =>
      val (l, h) = low_high(a, b)
      whenever(h < Integer.MAX_VALUE) {
        val rv = UniformInt(Range(Inclusive(l), Inclusive(h)))
        val value = rv.eval
        value should be >= l
        value should be <= h
      }
    }
  }
  it should "fall into higher-side exclusive range" in {
    forAll { (a: Int, b: Int) =>
      val (l, h) = low_high(a, b)
      whenever(l < h) {
        val rv = UniformInt(Range(Inclusive(l), Exclusive(h)))
        val value = rv.eval
        value should be >= l
        value should be <= h
      }
    }
  }
  it should "fall into lower-side exclusive range" in {
    forAll { (a: Int, b: Int) =>
      val (l, h) = low_high(a, b)
      whenever(l < h && h < Integer.MAX_VALUE) {
        val rv = UniformInt(Range(Exclusive(l), Inclusive(h)))
        val value = rv.eval
        value should be >= l
        value should be <= h
      }
    }
  }
  it should "fall into exclusive range" in {
    forAll { (a: Int, b: Int) =>
      val (l, h) = low_high(a, b)
      whenever(h < Integer.MAX_VALUE && l + 1 < h) {
        val rv = UniformInt(Range(Exclusive(l), Exclusive(h)))
        val value = rv.eval
        value should be >= l
        value should be <= h
      }
    }
  }
  it should "reject invalid exclusive range" in {
    forAll { (a: Int) =>
      whenever(a < Integer.MAX_VALUE) {
        an[IllegalArgumentException] should be thrownBy {
          UniformInt(Range(Exclusive(a), Exclusive(a + 1)))
        }
      }
    }
  }
}
