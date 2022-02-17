package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PolynomialSpec extends AnyFlatSpec with Matchers {
  "Polynomials" should "be undefined" in {
    an[NotImplementedError] should be thrownBy Polynomial.fitness
  }
}
