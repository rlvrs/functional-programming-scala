package week6.aVectors

/**
  * The  Vector structure is better for bulk operations (map, foldLeft, filter).
  * The List structure is better for getting always the "head" or for recursive access.
  *
  * Check out the hierarchy for other collections.
  */
class Pairs {
  def isPrime(n: Int): Boolean = {
    (2 until n) forall (_ % n != 0)
  }

  // TODO: Comment bc this was extracted from a Scala Worksheet.
  val n = 7

  // Returns a Sequence of vectors of pairs
  (1 until n) map (i =>
    (1 until i) map (j => (i,j)))

  // Returns an Indexed Sequence of pairs (what we want)
  ((1 until n) map (i =>
    (1 until i) map (j => (i,j)))).flatten
  // OR
  (1 until n).flatMap(i =>
    (1 until i) map (j => (i, j)))

  // Returns an Indexed Sequence of the pairs which the sum is prime (what we want)
  (1 until n).flatMap(i =>
    (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))
  // OR
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}
