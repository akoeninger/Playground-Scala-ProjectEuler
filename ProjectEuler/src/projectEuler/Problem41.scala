package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/2/12
  * Time: 8:49 AM
  * What is the largest n-digit pandigital prime that exists?
  * == Change Log ==
  * 10/2/12 - initial creation
  * solved and submitted
  */
object Problem41 {
  def isPrime(n: Int): Boolean = BigInt(n).isProbablePrime(5)

  @tailrec
  def largestPandigitalPrime(s: String): String = {
    findPrimePandigitals(s) match {
      case Some(x) => x
      case None    => largestPandigitalPrime(s.drop(1))
    }
  }

  def findPrimePandigitals(s: String): Option[String] = {
    val pandigitals = s.permutations.toList
    pandigitals collectFirst {
      case n if isPrime(n.toInt) => n
    }
  }

  def main(args: Array[String]) {
    val p = largestPandigitalPrime("987654321")
    println("p = " + p)
  }

}
