package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 12/13/12
  * Time: 9:30 AM
  * Roman numerals
  * == Change Log ==
  * 12/13/12 - initial creation    \
  * solved and submitted
  */
object Problem89 {
  /*
  I = 1
  V = 5
  X = 10
  L = 50
  C = 100
  D = 500
  M = 1000

  Must be in descending order, in order to distinguish subtractive combinations
  Out of order numerals indicate subtraction "IV" for 4
  Subtractive Rules:
     i. Only I, X, and C can be used as the leading numeral in a subtractive pair
    ii. I can only be placed before V and X
   iii. X can only be placed before L and C
    iv. C can only be placed before D and M
  49 != IL, preferred is XLIX

var s = r
    romanNumerals.foldLeft(0) {
      (n, t) => {
        val l = s.length
        s = s.replaceAll(t._1, "")
        val c = (l - s.length) / t._1.length
        n + (c * t._2)
      }
    }
   */

  val romanNumerals: List[(String, Int)] = List("CM" -> 900, "M" -> 1000, "CD" -> 400, "D" -> 500, "XC" -> 90, "C" -> 100,
    "XL" -> 40, "L" -> 50, "IX" -> 9, "X" -> 10, "IV" -> 4, "V" -> 5, "I" -> 1)

  val romanDigits: Map[Int, String] = (for (i <- romanNumerals) yield i.swap).toMap

  val digitKeys = romanDigits.keysIterator.toList sortBy (x => -x)

  def toRoman(n: Int): String = {
    @tailrec
    def toR(n: Int, numeral: String): String = digitKeys.find(_ <= n) match {
      case Some(key) => toR(n - key, numeral + romanDigits(key))
      case None => numeral
    }
    toR(n, "")
  }

  def fromRoman(rn: String): Int = {
    @tailrec
    def convert(r: String, tail: List[(String, Int)], acc: Int): Int = tail match {
      case Nil => acc
      case _ => {
        val newR = r.replaceAll(tail.head._1, "")
        val c = (r.length - newR.length) / tail.head._1.length
        convert(newR, tail.tail, acc + (c * tail.head._2))
      }
    }
    convert(rn, romanNumerals, 0)
  }

  def main(args: Array[String]) {
    val file = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\roman.txt")
    val numerals = file.getLines().toList
    val simplifiedNumerals = numerals.map(r => toRoman(fromRoman(r)))

    val originalSize = numerals.map(_.length).sum
    val simplifiedSize = simplifiedNumerals.map(_.length).sum

    val result = originalSize - simplifiedSize
    println("result = " + result)


  }

}
