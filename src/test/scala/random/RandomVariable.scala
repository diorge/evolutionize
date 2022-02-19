package random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import cats._
import cats.implicits._

class RandomVariableSpec
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
  "mapping to Const" should "apply to the value" in {
    forAll { (a: Int) =>
      whenever(a < Integer.MAX_VALUE) {
        val rv = const(a)
        val value = rv.map(_ + 1).eval
        value should equal(a + 1)
      }
    }
  }
  it should "follow the identity law" in {
    forAll { (a: AnyVal) =>
      const(a).map(x => x).eval == const(a).eval
    }
  }
  it should "follow the composition law" in {
    val f = (x: Int) => x - 5
    val g = (x: Int) => x + 1
    forAll { (a: Int) =>
      const(a).map(x => f(g(x))).eval == const(a).map(f).map(g).eval
    }
  }
  it should "follow the monad left identity law" in {
    val f = (x: Int) => const(x + 1)
    forAll { (a: Int) =>
      whenever(a < Integer.MAX_VALUE) {
        Monad[RandomVariable].pure(a).flatMap(f).eval == f(a).eval
      }
    }
  }
  it should "follow the monad right identity law" in {
    forAll { (a: AnyVal) =>
      const(a).flatMap(Monad[RandomVariable].pure).eval == const(a).eval
    }
  }
  it should "follow the monad associativity law" in {
    forAll { (a: Int) =>
      val f = (x: Int) => const(x - 5)
      val g = (x: Int) => const(x + 1)
      val m = const(a)
      m.flatMap(f).flatMap(g).eval == m.flatMap(x => f(x).flatMap(g)).eval
    }
  }
  "evaluating a RV" should "call the PRNG" in {
    import java.util.random.RandomGenerator
    given prng: RandomGenerator = new java.util.Random {
      override def nextInt(min: Int, max: Int): Int = ???
    }
    val rv = UniformInt(Range(Inclusive(0), Inclusive(10)))
    an[NotImplementedError] should be thrownBy rv.eval
  }
  "not evaluating a RV" should "not call the PRNG" in {
    import java.util.random.RandomGenerator
    given prng: RandomGenerator = new java.util.Random {
      override def nextInt(min: Int, max: Int): Int = ???
    }
    val rv: RandomVariable[Int] =
      UniformInt(Range(Inclusive(0), Inclusive(10)))
    noException should be thrownBy rv.map(_ * 2).map(_ + 1)
  }
}
