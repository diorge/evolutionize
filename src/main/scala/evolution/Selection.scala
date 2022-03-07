package evolution

import cats._
import cats.implicits._

import random.RandomVariable
import random.given_Monad_RandomVariable

import representation._

trait PopulationSelection[G, P, Ord]:
  def select(
      pop: List[(G, P)]
  )(using Genotype[G], Phenotype[P, Ord]): List[(G, P)]

  def evolve(
      pop: List[(G, P)]
  )(using
      geno: Genotype[G],
      pheno: Phenotype[P, Ord],
      birth: Birth[G, P, Ord]
  ): RandomVariable[List[(G, P)]] =
    val mutations: RandomVariable[List[G]] =
      pop.map(_._1).map(geno.mutate).sequence
    val newGeneration = mutations.map(_.map(birth.birth))

    val allIndividuals = newGeneration.map(_ ++ pop)
    val remaining = allIndividuals.map(select)
    remaining

given keepHalf[G, P, Ord: Ordering]: PopulationSelection[G, P, Ord] with
  def select(
      pop: List[(G, P)]
  )(using geno: Genotype[G], pheno: Phenotype[P, Ord]): List[(G, P)] =
    pop.sortBy((g, p) => pheno.fitness(p)).take(pop.size / 2)

def top[P, Ord: Ordering](
    phenotypes: List[P]
)(using pheno: Phenotype[P, Ord]): P =
  phenotypes.sortBy(pheno.fitness).head
