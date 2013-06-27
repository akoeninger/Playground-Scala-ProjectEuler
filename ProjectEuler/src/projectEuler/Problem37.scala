package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/27/12
  * Time: 9:20 AM
  *
  * == Change Log ==
  * 8/27/12 - initial creation
  * solved and submitted
  */
object Problem37 {
  val primes = primeList(1000000) filter (p => p > 7)

  @tailrec
  def checkLeftToRight(p: Int): Boolean = p match {
    case n if n < 10                       => BigInt(p).isProbablePrime(3)
    case n if BigInt(p).isProbablePrime(3) => checkLeftToRight(n.toString.drop(1).toInt)
    case _                                 => false
  }

  @tailrec
  def checkRightToLeft(p: Int): Boolean = p match {
    case 0                                 => true
    case n if BigInt(p).isProbablePrime(3) => checkRightToLeft(n / 10)
    case _                                 => false
  }

  def main(args: Array[String]) {
    val truncatable = primes filter (p => checkLeftToRight(p) && checkRightToLeft(p))

    println(truncatable.toList)
    val r = truncatable.sum
    println("r = " + r)
  }
}
