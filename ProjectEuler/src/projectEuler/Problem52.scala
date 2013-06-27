package projectEuler

import annotation.tailrec

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/17/12
  * Time: 8:49 AM
 * solved and submitted
  */

object Problem52 {
  def containsSameDigits(n: Int, o: Int): Boolean = {
    val nDigits = toDigits(n)
    val oDigits = toDigits(o)

    nDigits forall { oDigits contains _ }
  }

  @tailrec
  def findCyclic(start: Int): Int = {
    val result = (start to start * 10 / 6) find (i => (2 to 6) forall (p => containsSameDigits(i, i * p)))
    result match {
      case Some(x) => x
      case None    => findCyclic(start * 10)
    }
  }

  def main(args: Array[String]) {
    println("result = " + findCyclic(10))
  }
}
