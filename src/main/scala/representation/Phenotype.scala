package representation

trait Phenotype[P, Ord: Ordering]:
  def fitness(x: P): Ord
