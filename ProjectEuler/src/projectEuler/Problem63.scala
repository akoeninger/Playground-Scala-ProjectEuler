package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 10/25/12
  * Time: 8:32 AM
  * How many n-digit positive integers exists which are also an nth power?
  * solved and submitted
  * == Change Log ==
  * 10/25/12 - initial creation
  */
object Problem63 {
  def main(args: Array[String]) {
    println("result = " + loopRec(1,0,0))
  }

  @tailrec
  def loopRec(n: Int, lower: Int, res: Int): Int = lower match {
    case 10 => res
    case x  => loopRec(
      n + 1,
      math.ceil(math.pow(10, (n - 1.0) / n)).toInt,
      res + (10 - math.ceil(math.pow(10, (n - 1.0) / n)).toInt))
  }
}
