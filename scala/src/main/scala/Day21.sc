// Input parsing.
case class Item(name: String, cost: Int, damage: Int = 0, armor: Int = 0 )
val weapons = Seq(
  Item("Dagger", 8, 4, 0),
  Item("Shortsword", 10, 5, 0),
  Item("Warhammer", 25, 6, 0),
  Item("Longsword", 40, 7, 0),
  Item("Greataxe", 74, 8, 0)
)
val armor = Seq(
  Item("No armor", 0, 0, 0),
  Item("Leather", 13, 0, 1),
  Item("Chainmail", 31, 0, 2),
  Item("Splintmail", 53, 0, 3),
  Item("Bandedmail", 75, 0, 4),
  Item("Platemail", 102, 0, 5)
)
val rings = Seq(
  Item("Damage +1", 25, 1, 0),
  Item("Damage +2", 50, 2, 0),
  Item("Damage +3", 100, 3, 0),
  Item("Defence +1", 20, 0, 1),
  Item("Defence +2", 40, 0, 2),
  Item("Defence +3", 80, 0, 3)
)

case class Player(name: String, hitPoints: Int, damage: Int, armor: Int)

val boss = Player("Boss", 109, 8, 2)

// First puzzle.
val ringPairs = rings
  .permutations
  .toList
  .map {
    case r1 :: r2 :: tail => Item( r1.name +  " - " + r2.name, r1.cost + r2.cost, r1.damage + r2.damage, r1.armor + r2.armor)
  }
val withNoRings: Seq[Item] = Item("No ring", 0, 0, 0) +: rings
val totalRings = withNoRings ++ ringPairs
val totalRingCount = totalRings.size
val elements = for {
  w <- weapons
  a <- armor
  r <- totalRings
} yield Item(
  w.name + " - " + a.name + " - " + r.name,
  w.cost + a.cost + r.cost,
  w.damage + a.damage + r.damage,
  w.armor + a.armor + r.armor
)

def fight( p1: Player, p2: Player) : Player = {
  val hitPoints =  attack(p2, p1.damage)
  hitPoints match {
    case 0 => p1
    case _ => fight(Player(p2.name, hitPoints, p2.damage, p2.armor ), p1)
  }
}

def attack( p: Player, damage: Int) : Int = p match {
  case _ if p.hitPoints == 0 => 0
  case _ if p.armor >= damage => p.hitPoints - 1
  case _ if p.hitPoints < damage - p.armor => 0
  case _ => p.hitPoints - damage + p.armor
}

val fights = elements
              .par
              .map( e => {
                val you = Player("You", 100, e.damage, e.armor)
                (e.cost, fight(you, boss).name)
              })

val solution = fights
                .filter(_._2 == "You")
                .map(_._1)
                .min                          // 111


// Second puzzle.
val maxLost = fights
                .filter(_._2 == "Boss")
                .map(_._1)
                .max                          // 188
