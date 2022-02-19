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
  "random uniform integer" should "fall into inclusive range" in {
    forAll { (a: Int, b: Int) =>
      val l = scala.math.min(a, b)
      val h = scala.math.max(a, b)

      whenever(h < Integer.MAX_VALUE) {
        val rv = UniformInt(Range(Inclusive(l), Inclusive(h)))
        val value = rv.eval
        value should be >= l
        value should be <= h
      }
    }
  }
}
