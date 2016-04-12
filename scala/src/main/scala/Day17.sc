val input = "33\n14\n18\n20\n45\n35\n16\n35\n1\n13\n18\n13\n50\n44\n48\n6\n24\n41\n30\n42"

// Input parsing.
val containers = input.split("\n").map(_.toInt).toVector

val volume = 150

// First puzzle.
def calculate(buckets: Vector[Int], v: Int) : Vector[Vector[Int]] = {
  buckets.indices
  .flatMap( i => {
    val b = buckets(i)
    (b, v) match {
      case s if b > v => Vector(Vector())
      case s if b == v => Vector(Vector(b))
      case s if b < v => {
        val (_, end) = buckets.splitAt(i)
        calculate(end.tail, v - b)
        .filter( s => s.nonEmpty )
        .map( t => b +: t)
      }
    }
  }).toVector
}

val combinations = calculate(containers, volume)
val solution = combinations.size    // 1304

// Second puzzle.
val minBucketNumber = combinations.map(_.size).min

val minBucketCombinations = combinations.count(_.size == minBucketNumber) // 18
