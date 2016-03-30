val input = "3113322113"

// First puzzle.
val pattern = """((\d)\2*)""".r

def calculate(seed: String, counter: Int) : String  = {
  if ( counter == 0 ) {
    seed
  } else {
    val newSeed = pattern
                  .findAllMatchIn(seed)
                  .toList
                  .map( m => {
                    m.group(1).length + m.group(2)
                  })
                  .mkString
    calculate(newSeed, counter - 1)
  }

}

val length = calculate(input,40).length   // 329356

// Second puzzle.
val length50 = calculate(input,50).length // 4666278
