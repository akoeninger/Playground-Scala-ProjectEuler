package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/6/12
  * Time: 1:57 PM
  *
  * == Change Log ==
  * 9/6/12 - initial creation
  * solved and submitted
  */
object Problem48 {
  def lastTenDigits(x: Int): Long = BigInt(x).pow(x).toString().takeRight(10).toLong

  def main(args: Array[String]) {
    val seq = for (i <- 1 to 1000) yield lastTenDigits(i)
    val sum = seq.sum
    println("sum = " + sum.toString.takeRight(10))
  }
}
