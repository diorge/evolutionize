package random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RangeSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks {
  "bounds" should "accept integers" in {
    noException should be thrownBy {
      val inc = Inclusive(5)
      val exc = Exclusive(10)
    }
  }
  it should "accept doubles" in {
    noException should be thrownBy {
      val inc = Inclusive(5.3)
      val exc = Exclusive(10.5)
    }
  }
  "range" should "accept min < max" in {
    forAll { (a: Int, b: Int) =>
      whenever(a != b) {
        val lower = math.min(a, b)
        val higher = math.max(a, b)
        noException should be thrownBy {
          val ii = Range(Inclusive(lower), Inclusive(higher))
          val ie = Range(Inclusive(lower), Exclusive(higher))
          val ei = Range(Exclusive(lower), Inclusive(higher))
          val ee = Range(Exclusive(lower), Exclusive(higher))
        }
      }
    }
  }
  it should "accept min=max if both are inclusive" in {
    forAll { (v: Int) =>
      noException should be thrownBy Range(Inclusive(v), Inclusive(v))
    }
  }
  it should "reject min=max if either side is exclusive" in {
    forAll { (v: Int) =>
      an[IllegalArgumentException] should be thrownBy Range(
        Inclusive(v),
        Exclusive(v)
      )
      an[IllegalArgumentException] should be thrownBy Range(
        Exclusive(v),
        Inclusive(v)
      )
      an[IllegalArgumentException] should be thrownBy Range(
        Exclusive(v),
        Exclusive(v)
      )
    }
  }
}
