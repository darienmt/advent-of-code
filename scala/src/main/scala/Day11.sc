val input = "hepxcrrq"

// First puzzle.
def inc( s: String ) : String = {
  s
  .reverse
  .foldLeft(("", 1) )( (a, c) => {
    a._2 match {
      case 0 => ( a._1 + c, 0 )
      case _ => if ( c.toInt + a._2 > 122 ) {
        ( a._1 + "a", 1)
      } else {
        ( a._1 + (c.toInt + 1).toChar, 0)
      }
    }

  })
  match {
    case (o, 0) => o.reverse
    case (o, 1) => "a" + o.reverse
  }
}

def isValid(s: String) : Boolean =
  secondRule(s) && thirdRule(s) && firstRule(s)

def firstRule(s: String) : Boolean =
  s
  .sliding(3)
  .exists(s => {
    if (s.length != 3) {
      false
    } else {
      val v = s.map(_.toInt)
      (v(1) == v(0) + 1) && (v(2) == v(0) + 2)
    }
  })

def secondRule(s: String) : Boolean = {
  !s.contains("i") && !s.contains("o") && !s.contains("l")
}

def thirdRule(s: String) : Boolean = {
  s
  .sliding(2)
  .foldLeft[List[Char]](List())((a, v) => {
    if (v(0) == v(1) && !a.contains(v(0))) {
      v(0) :: a
    } else {
      a
    }
  })
  .size > 1
}

def calculate(s: String) : String = {
  if ( isValid(s) ) {
    s
  } else {
    calculate(inc(s))
  }
}

val solution = calculate(input)           // hepxxyzz

// Second puzzle.
val solution2 = calculate(inc(solution))  // heqaabcc
