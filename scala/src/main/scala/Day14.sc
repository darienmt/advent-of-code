val input = "Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.\nBlitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.\nRudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds.\nCupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds.\nDonner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds.\nDasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds.\nComet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds.\nPrancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds.\nDancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds."

// Input parsing.
val pattern = """([A-Z][a-z]+) can fly ([0-9]+) km\/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds.""".r

case class Reindeer(name: String, speed: Int, fly: Int, rest: Int)

val reindeer = input
                .split("\n")
                .map {
                        case pattern(name, speed, fly, rest) => Reindeer(name, speed.toInt, fly.toInt, rest.toInt)
                     }

// First puzzle.
case class ReindeerState(rd: Reindeer, isFlying: Boolean, howLong: Int, distance: Int)

def fly(rds: Array[Reindeer], iterations: Int) : Seq[(String,Int)] = {
  (1 to iterations)
  .foldLeft( rds.map(r => ReindeerState(r,false, 0, 0)) )( (a, i) => {
    a map nextState
  })
  .map( rs => (rs.rd.name, rs.distance))
}

def nextState( rdState: ReindeerState ) : ReindeerState = rdState match {
  case ReindeerState(r, false, 0, 0) => ReindeerState(r, true, 1, r.speed)
  case ReindeerState(r, false, n, d) if n < r.rest => ReindeerState(r, false, n + 1, d)
  case ReindeerState(r, false, n, d) if r.rest == n => ReindeerState(r, true, 1, d + r.speed)
  case ReindeerState(r, true, n, d) if n < r.fly => ReindeerState(r, true, n + 1, d + r.speed)
  case ReindeerState(r, true, n, d) if r.fly == n => ReindeerState(r, false, 1, d)
}

val solution = fly(reindeer, 2503).map( e => e._2 ).max  // 2655


// Second puzzle.
case class ReindeerScore(rdState: ReindeerState, score: Int)

def fly2(rds: Array[Reindeer], iterations: Int)  = {
  (1 to iterations)
  .foldLeft( rds.map(r => ReindeerScore(ReindeerState(r,false, 0, 0), 0 ) ) )( (a, _) => {
    val lineUp = a map( r => ReindeerScore( nextState(r.rdState), r.score) )
    val maxDistance = lineUp.map( r => r.rdState.distance ).max
    lineUp map {
      case score if score.rdState.distance == maxDistance => ReindeerScore(score.rdState, score.score + 1)
      case score => score
    }
  })
  .map( s => (s.rdState.rd.name, s.score))
}

val solution2 = fly2(reindeer, 2503).map( e => e._2 ).max   // 1059
