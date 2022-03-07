package random

import java.util.random.RandomGenerator

trait Uniform[T] extends RandomVariable[T]:
  def range: Range[T]

case class UniformInt(range: Range[Int]) extends Uniform[Int]:
  require(range match {
    case Range(Exclusive(_), Exclusive(_)) => range.range > 1
    case _                                 => true
  })

  def eval(using rng: RandomGenerator): Int =
    val lower = range.min.applyOnExclusive(_ + 1)
    val higher = range.max.applyOnInclusive(_ + 1)
    if lower == higher then lower else rng.nextInt(lower, higher)

def uniformInt(range: Range[Int]): RandomVariable[Int] = UniformInt(range)
