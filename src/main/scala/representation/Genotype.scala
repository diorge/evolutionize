package representation

import random.RandomVariable

trait Genotype[G]:
  def mutate(x: G): RandomVariable[G]
