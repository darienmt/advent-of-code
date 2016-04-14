val input = 29000000

// First puzzle.
def findHouse( i: Int, maxGifts: Int )(giftPerHouseCalculator: Int => Int) : Int =
  if ( giftPerHouseCalculator(i) > maxGifts ) {
    i
  } else {
    findHouse(i+1, maxGifts)(giftPerHouseCalculator)
  }

def factorize(n: Int): List[Int] =
  List(n,1) ++ (2 to Math.sqrt(n).toInt).flatMap {
                                                   case i if n % i == 0 && i != n / i => List(i, n/i)
                                                   case i if n % i == 0 => List(i)
                                                   case _ => List()
                                                 }


val solution = findHouse(1, input)(factorize(_).sum * 10)   // 665280

// Second puzzle.
def houseGifts2( i: Int ) : Int =
  factorize(i)
  .map {
         case x if i / x <= 50=> x * 11
         case _ => 0
       }
  .sum

val solution2 = findHouse(1, input)(houseGifts2) // 705600
