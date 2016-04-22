val input = "1\n3\n5\n11\n13\n17\n19\n23\n29\n31\n41\n43\n47\n53\n59\n61\n67\n71\n73\n79\n83\n89\n97\n101\n103" +
  "\n107\n109\n113"

// Input parsing.
val totalPackages = input.split("\n").map(_.toInt)

// Fist puzzle.
def findMin( p: Array[Int], compartmentCount: Int = 3) = {
  val compartmentWeight : Double = p.sum / compartmentCount
  var found = false
  var size = 1
  var iterator : Iterator[Set[Int]] = null
  while ( !found ) {
    iterator = p.toSet[Int].subsets(size).filter( _.sum == compartmentWeight)
    found = iterator.nonEmpty
    size += 1
  }
  iterator
    .toList
    .map( s => s.foldLeft(BigDecimal(1))( ( a, i ) => a * i ) )
    .min
}

val minQuantumEntanglement1 = findMin(totalPackages, 3)     // 11266889531


// Second puzzle.
val minQuantumEntanglement2 = findMin(totalPackages, 4)     // 77387711
