package random

import java.util.random.RandomGenerator

import cats._

trait RandomVariable[T] {
  def eval(using RandomGenerator): T
}

given RandomGenerator = java.util.Random()

final case class Const[A](value: A) extends RandomVariable[A] {
  def eval(using RandomGenerator) = value
}

def const[A](value: A): RandomVariable[A] = Const(value)

given Monad[RandomVariable] with
  def pure[A](value: A): RandomVariable[A] = Const(value)
  def flatMap[A, B](rv: RandomVariable[A])(
      func: A => RandomVariable[B]
  ): RandomVariable[B] = new RandomVariable[B] {
    def eval(using RandomGenerator): B = func(rv.eval).eval
  }
  def tailRecM[A, B](a: A)(
      func: A => RandomVariable[Either[A, B]]
  ): RandomVariable[B] = new RandomVariable[B] {
    def eval(using RandomGenerator): B = func(a).eval match {
      case Left(x)  => tailRecM(x)(func).eval
      case Right(x) => x
    }
  }

trait Uniform[T] extends RandomVariable[T] {
  def range: Range[T]
}

case class UniformInt(range: Range[Int]) extends Uniform[Int]:
  def eval(using rng: RandomGenerator): Int =
    val lower = range.min.applyOnExclusive(_ + 1)
    val higher = range.max.applyOnInclusive(_ + 1)
    if lower == higher then lower else rng.nextInt(lower, higher)
