package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 11/19/12
  * Time: 8:42 AM
  *
  * == Change Log ==
  * 11/19/12 - initial creation
  * 11/19/12 - solved, non-scala
  */
object Problem72 {
  def phiMemo(n: Int, f: Int => Int): Int = n match {
    case p if BigInt(p).isProbablePrime(5) => (p - 1)
    //    case p if isPrimePower(p) => math.round(p * (1 - (1.0 / primePower(p).get)))
    case p if p > 1 && p % 2 == 0 => if ((p / 2) % 2 == 0) 2 * f(p / 2) else f(p / 2)
    case _ => phi(n).toInt
  }

  val fastPhi = Memoize1.Y(phiMemo)

  def numSetElementsDesc(d: Int): Long = (d to 2 by -1).map(fastPhi(_).toLong).sum

  def numAscend(d: Int): Long = (2 to d).map(fastPhi(_).toLong).sum

//  def computeMultiples(interval: Int, limit: Int, ar: Array[Int]): Array[Int] = if

  def main(args: Array[String]) {
    val limit = 8
    val p = Array.range(0, limit + 1)
    val result = computeResult(limit, p, 2, 0L)

    println(result)
  }

  @tailrec
  def computeResult(limit: Int, p: Array[Int], index: Int, result: Long): Long = if (index <= limit) {
    var temp = p
    if (p(index) == index) {
      temp = setMultiples(index, limit, p)
    }
    computeResult(limit, p, index + 1, result + temp(index))
  } else {
    result
  }

  def setMultiples(i: Int, limit: Int, p: Array[Int]): Array[Int] = {
    for (j <- i to limit by i) {
      p(j) = p(j) / i * (i - 1)
    }
    p
  }
}
