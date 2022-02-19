package random

sealed trait Bound[T: Ordering] {
  def bound: T

  def applyOnInclusive(f: T => T): T = this match {
    case Inclusive(v) => f(v)
    case Exclusive(v) => v
  }
  def applyOnExclusive(f: T => T): T = this match {
    case Inclusive(v) => v
    case Exclusive(v) => f(v)
  }
}

final case class Inclusive[T: Ordering](bound: T) extends Bound[T]
final case class Exclusive[T: Ordering](bound: T) extends Bound[T]

case class Range[T: Ordering](min: Bound[T], max: Bound[T]):
  private def validBound = {
    import scala.math.Ordering.Implicits._
    (min, max) match {
      case (Inclusive(left), Inclusive(right)) => left <= right
      case _                                   => min.bound < max.bound
    }
  }
  require(validBound)

  def range(using Numeric[T]): Double =
    import scala.math.Numeric.Implicits._
    max.bound.toDouble - min.bound.toDouble
