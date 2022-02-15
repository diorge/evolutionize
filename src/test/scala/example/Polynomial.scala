package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PolynomialSpec extends AnyFlatSpec with Matchers {
  "Polynomials" should "be undefined" in {
    a[NotImplementedError] should be thrownBy Polynomial.fitness
  }
}
