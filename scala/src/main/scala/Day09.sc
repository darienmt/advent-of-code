val input = "AlphaCentauri to Snowdin = 66\nAlphaCentauri to Tambi = 28\nAlphaCentauri to Faerun = 60\nAlphaCentauri to Norrath = 34\nAlphaCentauri to Straylight = 34\nAlphaCentauri to Tristram = 3\nAlphaCentauri to Arbre = 108\nSnowdin to Tambi = 22\nSnowdin to Faerun = 12\nSnowdin to Norrath = 91\nSnowdin to Straylight = 121\nSnowdin to Tristram = 111\nSnowdin to Arbre = 71\nTambi to Faerun = 39\nTambi to Norrath = 113\nTambi to Straylight = 130\nTambi to Tristram = 35\nTambi to Arbre = 40\nFaerun to Norrath = 63\nFaerun to Straylight = 21\nFaerun to Tristram = 57\nFaerun to Arbre = 83\nNorrath to Straylight = 9\nNorrath to Tristram = 50\nNorrath to Arbre = 60\nStraylight to Tristram = 27\nStraylight to Arbre = 81\nTristram to Arbre = 90"

// Input parsing.
case class Path(from: String, to: String, distance: Int )

def parse(raw: String) : (Seq[String], Seq[Path]) = {
  val pattern = """(.+) to (.+) = ([0-9]+)""".r
  val p = raw
          .split('\n')
          .map {
                 case pattern(from, to, distance) => Path(from, to, distance.toInt )
               }
  ((p.map( _.from ) ++ p.map( _.to )).distinct, p)
}

val (nodes, edges) = parse(input)

// First puzzle.
def distance( nodes: Seq[String], edges: Seq[Path] ) =
  nodes
    .permutations
    .toList
    .map{
          _.sliding(2)
           .toList
           .map{
                 case Seq(first, second) => {
                   edges
                        .find(
                               path => (path.from == first && path.to == second)
                                        ||
                                       (path.to == first && path.from == second)
                             )
                        .map( _.distance ).getOrElse(Int.MaxValue)
                 }
               }
        .sum
    }

val minLength = distance(nodes, edges).min     // 141

// Second puzzle.
val maxLength = distance(nodes, edges).max     // 736

