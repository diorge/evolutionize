package example

import cats._
import cats.implicits._

import random._
import random.given_Monad_RandomVariable
import random.given_RandomGenerator
import random.{RandomVariable => RV}
import java.util.random.RandomGenerator

final case class Point(x: Int, y: Int)

def mutate(p: Point): RV[Point] =
  for {
    which <- uniformInt(Range(Inclusive(0), Inclusive(1)))
    change <- uniformInt(Range(Inclusive(-10), Inclusive(10)))
  } yield
    if which == 0 then Point(p.x + change, p.y)
    else Point(p.x, p.y + change)

def fitness(p: Point): Double =
  // manhattan distance to origin
  math.abs(p.x) + math.abs(p.y)

def select(pop: List[Point], amount: Int): List[Point] =
  // minimizes
  pop.sortBy(fitness).take(amount)

def evolve(pop: List[Point]): RV[List[Point]] =
  val mutations = pop.map(mutate).sequence
  val elite = mutations.map(m => select(m ++ pop, pop.size))
  elite

def evolutionLoop(iters: Int)(pop: List[Point]): RV[Point] =
  require(iters >= 0)
  if iters == 0 then const(select(pop, 1).head)
  else evolve(pop) >>= evolutionLoop(iters - 1)

@main
def origin(): Unit =
  val popSize = 500
  val iterations = 10
  val radius = 500

  def randomPoint(): RV[Point] =
    for {
      x <- uniformInt(Range(Inclusive(-radius), Inclusive(radius)))
      y <- uniformInt(Range(Inclusive(-radius), Inclusive(radius)))
    } yield Point(x, y)

  val initialPop = (1 to popSize).map(_ => randomPoint()).toList.sequence
  val winner = initialPop >>= evolutionLoop(iterations)

  println(winner.eval)
