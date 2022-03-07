package evolution

import cats._
import cats.implicits._

import random.{const, RandomVariable}
import random.given_Monad_RandomVariable

import representation._

def countEvolutionLoop[G, P, Ord](iterations: Int)(initialPopulation: List[G])(
    using
    selector: PopulationSelection[G, P, Ord],
    birth: Birth[G, P, Ord],
    geno: Genotype[G],
    pheno: Phenotype[P, Ord]
): RandomVariable[List[P]] =
  require(iterations >= 0)
  def evoLoop(it: Int)(p: List[(G, P)]): RandomVariable[List[(G, P)]] =
    if it == 0 then const(p)
    else selector.evolve(p) >>= evoLoop(it - 1)

  evoLoop(iterations)(initialPopulation.map(birth.birth))
    .map(_.map(_._2))
