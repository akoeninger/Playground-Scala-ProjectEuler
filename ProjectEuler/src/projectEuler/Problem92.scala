package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 12/12/12
  * Time: 12:01 PM
  *
  * == Change Log ==
  * 12/12/12 - initial creation
  * solved and submitted
  */
object Problem92 {
  def squareSum(x: Int): Int = {
    @tailrec
    def recurse(n: Int, sum: Int): Int = n match {
      case 0 => sum
      case _ => recurse(n / 10, sum + (n % 10) * (n % 10))
    }

    recurse(x, 0)
  }

  @tailrec
  def digitString(x: Int): Int = x match {
    case 1 => 0
    case 89 => 1
    case _ => digitString(squareSum(x))
  }

  def main(args: Array[String]) {
    val r = for (x <- 1 until 10000000) yield digitString(x)

    println(r.sum)
  }
}

