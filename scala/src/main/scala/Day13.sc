val input = "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 81 happiness units by sitting next to Carol.\nAlice would lose 42 happiness units by sitting next to David.\nAlice would gain 89 happiness units by sitting next to Eric.\nAlice would lose 89 happiness units by sitting next to Frank.\nAlice would gain 97 happiness units by sitting next to George.\nAlice would lose 94 happiness units by sitting next to Mallory.\nBob would gain 3 happiness units by sitting next to Alice.\nBob would lose 70 happiness units by sitting next to Carol.\nBob would lose 31 happiness units by sitting next to David.\nBob would gain 72 happiness units by sitting next to Eric.\nBob would lose 25 happiness units by sitting next to Frank.\nBob would lose 95 happiness units by sitting next to George.\nBob would gain 11 happiness units by sitting next to Mallory.\nCarol would lose 83 happiness units by sitting next to Alice.\nCarol would gain 8 happiness units by sitting next to Bob.\nCarol would gain 35 happiness units by sitting next to David.\nCarol would gain 10 happiness units by sitting next to Eric.\nCarol would gain 61 happiness units by sitting next to Frank.\nCarol would gain 10 happiness units by sitting next to George.\nCarol would gain 29 happiness units by sitting next to Mallory.\nDavid would gain 67 happiness units by sitting next to Alice.\nDavid would gain 25 happiness units by sitting next to Bob.\nDavid would gain 48 happiness units by sitting next to Carol.\nDavid would lose 65 happiness units by sitting next to Eric.\nDavid would gain 8 happiness units by sitting next to Frank.\nDavid would gain 84 happiness units by sitting next to George.\nDavid would gain 9 happiness units by sitting next to Mallory.\nEric would lose 51 happiness units by sitting next to Alice.\nEric would lose 39 happiness units by sitting next to Bob.\nEric would gain 84 happiness units by sitting next to Carol.\nEric would lose 98 happiness units by sitting next to David.\nEric would lose 20 happiness units by sitting next to Frank.\nEric would lose 6 happiness units by sitting next to George.\nEric would gain 60 happiness units by sitting next to Mallory.\nFrank would gain 51 happiness units by sitting next to Alice.\nFrank would gain 79 happiness units by sitting next to Bob.\nFrank would gain 88 happiness units by sitting next to Carol.\nFrank would gain 33 happiness units by sitting next to David.\nFrank would gain 43 happiness units by sitting next to Eric.\nFrank would gain 77 happiness units by sitting next to George.\nFrank would lose 3 happiness units by sitting next to Mallory.\nGeorge would lose 14 happiness units by sitting next to Alice.\nGeorge would lose 12 happiness units by sitting next to Bob.\nGeorge would lose 52 happiness units by sitting next to Carol.\nGeorge would gain 14 happiness units by sitting next to David.\nGeorge would lose 62 happiness units by sitting next to Eric.\nGeorge would lose 18 happiness units by sitting next to Frank.\nGeorge would lose 17 happiness units by sitting next to Mallory.\nMallory would lose 36 happiness units by sitting next to Alice.\nMallory would gain 76 happiness units by sitting next to Bob.\nMallory would lose 34 happiness units by sitting next to Carol.\nMallory would gain 37 happiness units by sitting next to David.\nMallory would gain 40 happiness units by sitting next to Eric.\nMallory would gain 18 happiness units by sitting next to Frank.\nMallory would gain 7 happiness units by sitting next to George."

// Input parsing.
case class Happiness(name:String, gain:Int, other: String)

val pattern = """([A-Z][a-z]+) would ([a-z]+) ([0-9]+) happiness units by sitting next to ([A-Z][a-z]+)\.""".r
val happiness = input
                .split("\n")
                .map {
                       case pattern(name, "gain", value, other ) => Happiness(name, value.toInt, other)
                       case pattern(name, "lose", value, other ) => Happiness(name, -value.toInt, other)
                     }

// Fist puzzle.
def maxHappiness( h: Seq[Happiness]): Int = {
  val people = h.map(_.name).distinct
  val solutions = people
                  .permutations
                  .toVector
                  .map( calculate( _, h ) )
  solutions.max
}

def calculate( table: Seq[String], h: Seq[Happiness] ) : Int = {
  val first = table.head
  val last = table.last
  val fullTable: List[String] = last :: (table.toList :+ first)
  fullTable
    .sliding(3)
    .map {
         case List(p1,p2,p3) => happinessBetween(p2, p1,h ) + happinessBetween(p2,p3,h)
    }
    .sum
}

def happinessBetween(name: String, other:String, aHappiness:Seq[Happiness]) : Int =
  aHappiness
    .find( h => h.name == name && h.other == other )
    .map( _.gain )
    .getOrElse( Int.MinValue )

val solution = maxHappiness(happiness)   // 709

// Second puzzle.
val myHappiness = happiness.map(_.name).distinct.map( p => Happiness("Me", 0, p))

val includingMeSolution = maxHappiness(happiness ++ myHappiness)  // 668
