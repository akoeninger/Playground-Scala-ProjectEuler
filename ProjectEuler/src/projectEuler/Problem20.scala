package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 7/5/12
  * Time: 8:03 AM
 * solved and submitted
  */

/** Find the sum of digits of 100!
  *
  * Example: 10! = 10 x 9 x ... x 2 x 1 = 3628800,
  * the sum of the digits in 10! is 3+6+2+8+8+0+0 = 27.
  */
object Problem20 {

  def factorial(n: Int): BigInt = {
    def fact(x: BigInt, r: BigInt): BigInt = if (x < 1) r else fact(x - 1, r * x)
    fact(BigInt(n), BigInt(1))
  }

  def digitSum(n: BigInt): BigInt = n.toString().view.map(_.asDigit).sum

  def main(args: Array[String]) {
    println(digitSum(factorial(100)))
  }
}
