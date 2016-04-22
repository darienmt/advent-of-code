// First, only, and last puzzle.
def findValue(x: Int, y: Int, initValue: BigInt, f: BigInt => BigInt) : BigInt = {
  var found = false
  val calculator = new Calculator(initValue, f)
  var value : BigInt = 0
  while ( !found ) {
    val ( v, (xx, yy) ) = calculator.next()
    if ( xx == x - 1 && yy == y - 1 ) {
      found = true;
      value = v
    }
  }
  value
}

class Calculator(initValue: BigInt, f: BigInt => BigInt) extends Iterator[(BigInt,(Int,Int))] {

  var lastValue = initValue

  var lastCoordinates = (0,0)

  override def hasNext: Boolean = true

  override def next(): (BigInt, (Int, Int)) = {
    val next = f(lastValue)
    val ( nextX, nextY ) = lastCoordinates match {
      case (x, 0) => (0, x + 1)
      case (x, y) => (x + 1, y - 1)
    }
    lastValue = next
    lastCoordinates = ( nextX, nextY )
    (next, lastCoordinates)
  }
}

val solution1 = findValue(3083, 2978, 20151125, v => (v * 252533) % 33554393 )   // 2650453
