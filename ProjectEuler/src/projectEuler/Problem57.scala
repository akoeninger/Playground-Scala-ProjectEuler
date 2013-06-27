package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/21/12
  * Time: 2:06 PM
  * finished, submitted
  * == Change Log ==
  * 9/21/12 - initial creation
  */
object Problem57 {
  @tailrec
  def expansion(n: BigInt, d: BigInt, limit: Int, result: Int): Int = limit match {
    case 0                     => result
    case x if moreDigits(n, d) => expansion(n + 2 * d, n + d, limit - 1, result + 1)
    case _                     => expansion(n + 2 * d, n + d, limit - 1, result)
  }

  def moreDigits(n: BigInt, d: BigInt): Boolean = n.toString().length > d.toString().length

  def main(args: Array[String]) {
    val start = System.currentTimeMillis()
    val result = expansion(BigInt(3), BigInt(2), 1000, 0)
    val time = System.currentTimeMillis() - start

    println("result = " + result)
    println("time = " + time)
  }

}
