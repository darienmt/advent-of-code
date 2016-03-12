val input = "ckczppom"

//First puzzle.
import java.security.MessageDigest

def md5(s: String) : String = {
  MessageDigest.getInstance("MD5").digest(s.getBytes).map( "%02X" format _).mkString
}

def findNumber( key: String, prefix: String ) = {
  var l: BigDecimal = 1
  while ( !md5(key + l).startsWith(prefix) ) {
    l = l + 1
  }
  l
}

val theNumber = findNumber(input, "00000") //117946

// Second puzzle.

val theSecondNumber = findNumber(input,"000000") // 3938038
