package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
 * User: dmak83
 * Date: 12/4/12
 * Time: 10:01 AM
 * solved and submitted
 * == Change Log == 
 * 12/4/12 - initial creation
 */
object Problem113 {
  def Factorial(x: BigInt): BigInt = {
    @tailrec
    def f(x: BigInt, r: BigInt): BigInt = if (x <= 1) r else f(x - 1, r * x)

    f(x, BigInt(1))
  }

  def binomial(n: Int, k: Int): BigInt = {
    val bigN = BigInt(n)
    val bigK = BigInt(k)

    Factorial(bigN) / (Factorial(bigK) * Factorial(bigN - bigK))
  }

  def main(args: Array[String]) {
    println(binomial(110,10) + binomial(109,9) - 1002)
  }

}
