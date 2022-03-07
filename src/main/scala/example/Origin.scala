package example

import cats._
import cats.implicits._

import evolution._
import random._
import representation._

import evolution.keepHalf
import random.given_Monad_RandomVariable
import random.given_RandomGenerator
import representation.identityBirth

final case class Point(x: Int, y: Int)

given Genotype[Point] with
  def mutate(p: Point): RandomVariable[Point] =
    for {
      which <- uniformInt(Range(Inclusive(0), Inclusive(1)))
      change <- uniformInt(Range(Inclusive(-10), Inclusive(10)))
    } yield
      if which == 0 then Point(p.x + change, p.y)
      else Point(p.x, p.y + change)

given Phenotype[Point, Double] with
  def fitness(p: Point): Double =
    // manhattan distance to origin
    math.abs(p.x) + math.abs(p.y)

@main
def origin(): Unit =
  val popSize = 500
  val iterations = 10
  val radius = 500

  val initialPop = firstGeneration(popSize) {
    for {
      x <- uniformInt(Range(Inclusive(-radius), Inclusive(radius)))
      y <- uniformInt(Range(Inclusive(-radius), Inclusive(radius)))
    } yield Point(x, y)
  }

  val lastGen =
    initialPop >>= countEvolutionLoop[Point, Point, Double](iterations)

  val winner = lastGen.map(top[Point, Double])

  println(winner.eval)
