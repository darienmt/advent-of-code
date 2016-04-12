val input = "Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2\nSprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9\nCandy: capacity -1, durability 0, flavor 4, texture 0, calories 1\nChocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8"

// Input parsing.
case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories:Int)

val pattern = """([A-Z][a-z]+): capacity (-?[0-9]+), durability (-?[0-9]+), flavor (-?[0-9]+), texture (-?[0-9]+), calories (-?[0-9]+)""".r
val ingredients = input
                  .split("\n")
                  .map {
                         case pattern(
                                        name,
                                        capacity,
                                        durability,
                                        flavor,
                                        texture,
                                        calories
                                     ) => Ingredient(
                                                      name,
                                                      capacity.toInt,
                                                      durability.toInt,
                                                      flavor.toInt,
                                                      texture.toInt,
                                                      calories.toInt
                                                    )
                       }

// First puzzle.
case class CookieScore(capacity: Int = 0, durability: Int = 0, flavor: Int = 0, texture: Int = 0, calories:Int = 0)

object CookieScore {
  def apply(v: List[Int]) : CookieScore = v match {
    case List(capacity, durability, flavor, texture, calories) => CookieScore(capacity, durability, flavor, texture, calories)
  }
}


def scoreSummary(cookie: CookieScore) : Int =
  cookie.capacity * cookie.durability * cookie.flavor * cookie.texture

def calculateCookies(ings: Array[Ingredient]):  Seq[CookieScore] =
  generateNumbers(100, ings.length)
  .filter( s=> s.sum == 100)
  .map( scoreRaw(_, ings))
  .map( x => {
    CookieScore(x.map(_.max(0)))
  })

def scoreRaw( r: Seq[Int], ings: Array[Ingredient]) : List[Int] =
  (r zip ings)
  .foldLeft(List(0,0,0,0,0))( (a, i) => {
    val ( count, ing ) = i
    val features = ing.productIterator.toList.drop(1).asInstanceOf[List[Int]]
    a zip features map {
     case ( s, f ) => s + count*f
    }
  })

def generateNumbers(limit: Int, iteration: Int ): Seq[Seq[Int]] = iteration match {
  case 1 => (0 to limit).map(Seq(_))
  case x => {
    for {
      i <- 0 to limit
      c <- generateNumbers(limit - i, iteration - 1)  if limit > 0
    } yield c :+ i
  }
}

val cookies = calculateCookies(ingredients)
val maxScore = cookies.map(scoreSummary).max // 222870

// Second puzzle.
val maxScore500 = cookies
                    .filter(_.calories == 500)
                    .map(scoreSummary)
                    .max                  // 117936
