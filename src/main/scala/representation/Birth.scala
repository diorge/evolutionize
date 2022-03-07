package representation

import cats._
import cats.implicits._

import random.RandomVariable
import random.given_Monad_RandomVariable

trait Birth[G, P, Ord]:
  def birth(genotype: G)(using Genotype[G], Phenotype[P, Ord]): (G, P)

given identityBirth[T, Ord]: Birth[T, T, Ord] with
  def birth(x: T)(using Genotype[T], Phenotype[T, Ord]) = (x, x)

def firstGeneration[G, P, Ord](
    populationSize: Int
)(generator: => RandomVariable[G])(using
    g: Genotype[G],
    p: Phenotype[P, Ord]
): RandomVariable[List[G]] =
  (1 to populationSize).map(_ => generator).toList.sequence
