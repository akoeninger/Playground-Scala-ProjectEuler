package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 2/7/13
  * Time: 9:20 AM
  *
  * == Change Log ==
  * 2/7/13 - initial creation
  */
object Problem413 {
  def main(args: Array[String]) {

    println(countChildNumbers(10L))
  }

  def produceSubStrings(number: String) = (1 to number.length) flatMap {
    i => number.sliding(i)
  }

  def checkSubStrings(number: String, subStrings: Seq[String]) = {
    val n = number.length
    subStrings.count(s => s.toInt % n == 0)
  }

  def countChildNumbers(n: Long): Int = {
    @tailrec
    def recurse(num: Long, result: Int): Int = if (num <= 1L) {
      result
    } else {
      val sub = produceSubStrings(num.toString)
      val count = if (checkSubStrings(num.toString, sub) == 1) 1 else 0

      recurse(num - 1L, result + count)
    }

    recurse(n, 0)
  }
}
