val input = "Al => ThF\nAl => ThRnFAr\nB => BCa\nB => TiB\nB => TiRnFAr\nCa => CaCa\nCa => PB\nCa => PRnFAr\nCa => SiRnFYFAr\nCa => SiRnMgAr\nCa => SiTh\nF => CaF\nF => PMg\nF => SiAl\nH => CRnAlAr\nH => CRnFYFYFAr\nH => CRnFYMgAr\nH => CRnMgYFAr\nH => HCa\nH => NRnFYFAr\nH => NRnMgAr\nH => NTh\nH => OB\nH => ORnFAr\nMg => BF\nMg => TiMg\nN => CRnFAr\nN => HSi\nO => CRnFYFAr\nO => CRnMgAr\nO => HP\nO => NRnFAr\nO => OTi\nP => CaP\nP => PTi\nP => SiRnFAr\nSi => CaSi\nTh => ThCa\nTi => BP\nTi => TiTi\ne => HF\ne => NAl\ne => OMg"
val myMolecule = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"

// Input parsing.
def parseReplacements( s: String ) : List[(String, String)] = {
  val pattern = """(.+) => (.+)""".r
  s.split("\n")
  .map {
         case pattern(k,v) => (k,v)
       }.toList
}
val allReplacements = parseReplacements(input)

// First puzzle.
def calculate( molecule: String, replacements: List[(String,String)] ) : List[String] =
  replacements
  .flatMap( replaceOptions(molecule, _) )

def replaceOptions( molecule: String, replacement: (String, String) ) : List[String] =
  (0 until molecule.length)
  .filter(molecule.startsWith(replacement._1, _))
  .map{
        case 0 => replacement._2 + molecule.substring( replacement._1.length )
        case i => molecule.substring(0, i) + replacement._2 + molecule.substring(i + replacement._1.length)
      }
  .toList

val solution = calculate(myMolecule, allReplacements).distinct.size    // 509

// Second puzzle.
def calculate2( molecule: String, replacements: List[(String,String)] ) = {
  val translated = molecule.replace("Rn","(").replace("Y", ",").replace("Ar",")")
  val tokens = replacements.map(_._1.reverse).distinct
  val (t, p, c) = count(translated.reverse.toList,(0,0,0), tokens)
  t - p - 2*c -1
}

def count( s: List[Char], counters: (Int,Int,Int), tokens: List[String]) : (Int,Int,Int) = {
  val (tc, pc, cc) = counters
  s match {
    case Nil => counters
    case ')' :: tail => count( tail, ( tc + 1, pc + 1, cc ), tokens)
    case '(' :: tail => count( tail, ( tc + 1, pc + 1, cc ), tokens)
    case ',' :: tail => count( tail, ( tc + 1, pc, cc + 1 ), tokens)
    case c1 :: tail if tokens.contains(c1.toString) => count( tail, ( tc + 1, pc, cc ), tokens)
    case c1 :: c2 :: tail if tokens.contains(c1.toString + c2.toString) => count( tail, ( tc + 1, pc, cc ), tokens)
    case _ :: tail => count(tail, ( tc + 1, pc, cc ), tokens)
  }
}

val solution2 = calculate2(myMolecule, allReplacements)             // 195


