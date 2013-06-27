package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/29/12
  * Time: 8:28 AM
  *
  * Find the n^th^ digit of the fractional part of the irrational number.
  * == Change Log ==
  * 8/29/12 - initial creation
  * solved and submitted
  */
object Problem40 {


  def maxDigit(n: Int): Int = {
    if (n <= 1) {
      return 9
    }
    val magnitude = math.ceil(math.log10(n)).toInt
    val offsets = for (n <- magnitude until 0 by -1) yield (math.pow(10, n - 1).toInt - 1)
    val digitRange = (math.pow(10, magnitude).toInt - 1) * magnitude

    digitRange - offsets.sum
  }

  //  def nthDigit(d: Int): Int = {
  //    @tailrec
  //    def builder(n: Int, digits: String): Int =
  //      if (digits.size >= d) {
  //        println(digits)
  //        digits.charAt(d-1).asDigit
  //      } else {
  //        builder(n + 1, digits + n)
  //      }
  //    builder(1, "")
  //  }

  def nthDigit(d: Int): Int = {
    if (d < 10) {
      d
    } else {
      val magnitude = math.ceil(math.log10(d)).toInt
      val lowerBound = maxDigit(d / 10)
      val startPoint = d - lowerBound
      val offset = (math.pow(10, magnitude - 1).toInt) - 1
      val remainder = startPoint % magnitude
      val num = (startPoint / magnitude) + offset

      if (remainder > 0) {
        (num + 1).toString.charAt(remainder - 1).asDigit
      } else {
        num.toString.last.asDigit
      }
    }
  }

  def main(args: Array[String]) {
    println(nthDigit(1) * nthDigit(10) * nthDigit(100) * nthDigit(1000) * nthDigit(10000) * nthDigit(100000) * nthDigit(1000000))
  }
}
