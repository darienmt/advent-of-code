val input = "jio a, +16\ninc a\ninc a\ntpl a\ntpl a\ntpl a\ninc a\ninc a\ntpl a\ninc a\ninc a\ntpl a\ntpl a\ntpl " +
  "a\ninc a\njmp +23\ntpl a\ninc a\ninc a\ntpl a\ninc a\ninc a\ntpl a\ntpl a\ninc a\ninc a\ntpl a\ninc " +
  "a\ntpl a\ninc a\ntpl a\ninc a\ninc a\ntpl a\ninc a\ntpl a\ntpl a\ninc a\njio a, +8\ninc b\njie a, " +
  "+4\ntpl a\ninc a\njmp +2\nhlf a\njmp -7"

// Input parsing.
case class State( next : Int = 0, regs : Map[String, BigInt] = Map( "a" -> 0, "b" -> 0))

type Instruction = State => State

def instructionCreator(r: String, regModifier: BigInt => BigInt = r => r, nextModified: (Int, BigInt) => Int = ( n, v ) => n + 1 ) : Instruction = {
  s => {
    val rVal = s.regs(r)
    s.copy( next = nextModified(s.next, rVal), regs = s.regs + ( r -> regModifier(rVal) ) )
  }
}

def hlf(r: String) : Instruction = instructionCreator( r, regModifier = _ / 2 )
def tpl(r: String) : Instruction = instructionCreator( r, regModifier = _ * 3 )
def inc(r: String) : Instruction = instructionCreator( r, regModifier = _ + 1 )
def jmp(offset: Int) : Instruction = instructionCreator( "a", nextModified = ( n, v ) => n + offset )
def jie(r: String, offset: Int) : Instruction = instructionCreator(r, nextModified = ( n, v ) => if ( v % 2 == 0 ) n + offset else n + 1)
def jio(r: String, offset: Int) : Instruction = instructionCreator(r, nextModified = ( n, v ) => if ( v == 1 ) n + offset else n + 1)

val instructions = input
                    .split("\n")
                    .map ( s => s.toList match {
                      case 'h' :: 'l' :: 'f' :: ' ' :: tail => hlf( tail.mkString )
                      case 't' :: 'p' :: 'l' :: ' ' :: tail => tpl( tail.mkString )
                      case 'i' :: 'n' :: 'c' :: ' ' :: tail => inc( tail.mkString )
                      case 'j' :: 'm' :: 'p' :: ' ' :: tail => jmp( tail.mkString.toInt )
                      case 'j' :: 'i' :: 'e' :: ' ' :: tail => {
                        val Array(r, offset) = tail.mkString.split(",")
                        jie(r, offset.trim.toInt )
                      }
                      case 'j' :: 'i' :: 'o' :: ' ' :: tail => {
                        val Array(r, offset) = tail.mkString.split(",")
                        jio(r, offset.trim.toInt )
                      }
                      case i => throw new Exception("Unknown instruction " + i.mkString)
                    })

// First puzzle.
def execute(state: State = State())( implicit code: Array[Instruction] ) : State = {
  var myState = state
  val n: Int = code.length
  println(n)
  while (myState.next < n ) {
    println(myState.next)
    myState = code(myState.next)(myState)
  }
  myState
}

var solution1 = execute()(instructions) // 170

// Second puzzle.
var solution2 = execute(State( regs = Map("a" -> 1, "b" -> 0)))(instructions) // 247
