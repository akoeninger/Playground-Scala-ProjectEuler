package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/4/12
  * Time: 8:32 AM
  * finished, submitted
  * == Change Log ==
  * 10/4/12 - initial creation
  */
object Problem56 {

  def sumDigits(a: Int, b: Int): Int = a match {
    case x if a % 10 == 0 => x / 10
    case _                => BigInt(a).pow(b).toString().map(_.asDigit).sum
  }

  def main(args: Array[String]) {
    def expList = for {
      a <- 2 until 100
      b <- 2 until 100
    } yield sumDigits(a, b)

    val maxSum = expList.max

    println("maxSum = " + maxSum)
  }

}
