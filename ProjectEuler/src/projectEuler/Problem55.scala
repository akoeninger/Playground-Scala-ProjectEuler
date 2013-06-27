package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/3/12
  * Time: 11:36 AM
  *
  * == Change Log ==
  * 10/3/12 - initial creation
  * finished, submitted
  */
object Problem55 {
  def addReverse(n: BigInt): BigInt = n + BigInt(n.toString().reverse)

  def isPalindrome(n: BigInt) = n.toString == n.toString().reverse


  def findLychrelNumber(n: Int): Option[Int] = {
    @tailrec
    def lychrelR(x: BigInt, i: Int): Option[Int] = i match {
      case 50                   => None
      case c if isPalindrome(x) => Some(c)
      case _                    => lychrelR(addReverse(x), i + 1)
    }
    lychrelR(addReverse(n), 1)
  }

  def main(args: Array[String]) {
    val lychrelMap = for (x <- 0 until 10000) yield findLychrelNumber(x)

    val count = lychrelMap.count(_ == None)
    println("count = " + count)
  }

}
