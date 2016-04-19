// Input parsing.
case class Spell(mana: Int, damage : Int = 0, heal: Int = 0, armor: Int = 0, turns: Int = 0, rechargeMana: Int = 0)
val spells = Map(
  "magic_missile" -> Spell(mana = 53, damage = 4),
  "drain" -> Spell(mana = 73, damage = 2, heal = 2),
  "shield" -> Spell(mana = 113, armor = 7, turns = 6),
  "poison" -> Spell(mana = 173, damage = 3, turns = 6),
  "recharge" -> Spell(mana = 229, turns = 5, rechargeMana = 101)
)

// First puzzle.
case class Game(
                 var winner: String = null,
                 player: Player = Player(),
                 boss: Boss = Boss(),
                 var poison: Int = 0,
                 var shield: Int = 0,
                 var recharge: Int = 0
               )

case class Boss(var hitPoints: Int, damage: Int)
object Boss {
  def apply() : Boss = Boss(58, 9)
  implicit def BossToString(b : Boss ) : String  = "boss"
}

case class Player(var hitPoints: Int, var mana: Int, var damage : Int = 0, var armor: Int = 0, var manaSpent: Int = 0)
object Player {
  def apply() : Player = Player(50, 500)
  implicit def PlayerToString(p : Player ) : String = "player"
}

def calculateMinMana( gameGenerator: Game => List[Game]) : Int = {
  val stack : scala.collection.mutable.Stack[Game] = new scala.collection.mutable.Stack()
  stack.push(Game())
  var min_mana_cost = Int.MaxValue
  var counter = 0
  while ( stack.nonEmpty ) {
    val game = stack.pop()
    counter += 1
    if ( game.player.manaSpent < min_mana_cost ) {
      if ( game.winner != null ) {
        if ( game.winner.toString == "player" ) {
          min_mana_cost = List(min_mana_cost, game.player.manaSpent).min
        }
      } else {
        gameGenerator(game).foreach( stack.push )
      }

    }
  }
  min_mana_cost
}

def generateGameStates(playersAttack: (Game, String) => Unit, bossAttack: Game => Unit )(gamestate: Game): List[Game] =
  spells.map( s => {
    val ( spell, _ ) = s
    val game = gamestate.copy( player = gamestate.player.copy(), boss = gamestate.boss.copy() )
    playersAttack(game, spell)
    if ( game.winner == null ) {
      bossAttack(game)
    }
    game
  })
    .filter( g => g.winner == null ||
      ( g.winner != null && g.winner.toString != "boss" )
    )
    .toList


val myGameGenerator = generateGameStates(playerPlays, bossPlays)(_)

def playerPlays(game: Game, spell: String): Unit = {
  if ( !isGameOver(game) ) {
    applySpells(game)

    if ( !isGameOver(game) ) {

      if ( game.player.mana < spells("magic_missile").mana ) {
        game.winner = game.boss
      } else {
        spell match {
          case "magic_missile" if game.player.mana >= spells(spell).mana => {
            game.player.mana -= spells(spell).mana
            game.player.manaSpent += spells(spell).mana
            game.boss.hitPoints -= spells(spell).damage
          }
          case "drain" if game.player.mana >= spells(spell).mana => {
            game.player.mana -= spells(spell).mana
            game.player.manaSpent += spells(spell).mana
            game.player.hitPoints += spells(spell).heal
            game.boss.hitPoints -= spells(spell).damage
          }
          case "shield" if game.shield == 0 && game.player.mana >= spells(spell).mana => {
            game.player.mana -= spells(spell).mana
            game.player.manaSpent += spells(spell).mana
            game.shield = spells(spell).turns
          }
          case "poison" if game.poison == 0 && game.player.mana >= spells(spell).mana => {
            game.player.mana -= spells(spell).mana
            game.player.manaSpent += spells(spell).mana
            game.poison = spells(spell).turns
          }
          case "recharge" if game.recharge == 0 && game.player.mana >= spells(spell).mana => {
            game.player.mana -= spells(spell).mana
            game.player.manaSpent += spells(spell).mana
            game.recharge = spells(spell).turns
          }
          case _ => game.winner = game.boss
        }
        isGameOver(game)
      }
    }
  }
}

def bossPlays(game: Game): Unit = {
  applySpells(game)
  if ( !isGameOver(game) ) {
    game.player.hitPoints -= List(game.boss.damage - game.player.armor, 1).max
    isGameOver(game)
  }
}

def isGameOver(game: Game): Boolean = {
  if ( game.player.hitPoints <= 0 ) {
    game.winner = game.boss
    true
  } else {
    if ( game.boss.hitPoints <= 0 ) {
      game.winner = game.player
      true
    } else {
      false
    }
  }
}
def applySpells(game: Game) : Game = {
  if ( game.shield > 0) {
    game.shield -= 1
    game.player.armor = spells("shield").armor
  } else {
    game.player.armor = 0
  }

  if ( game.poison > 0 ) {
    game.poison -= 1
    game.boss.hitPoints -= spells("poison").damage
  }
  if ( game.recharge > 0 ) {
    game.recharge -= 1
    game.player.mana += spells("recharge").rechargeMana
  }
  game
}

val minManaPuzzle1 = calculateMinMana(myGameGenerator) // 1269

// Second puzzle.
def playerPlays2(game: Game, spell: String) : Unit = {
  game.player.hitPoints -= 1
  if ( !isGameOver(game) ) {
    playerPlays(game, spell)
  }
}
val myGameGenerator2 = generateGameStates(playerPlays2, bossPlays)(_)

val minManaPuzzle2 = calculateMinMana(myGameGenerator2) // 1309
