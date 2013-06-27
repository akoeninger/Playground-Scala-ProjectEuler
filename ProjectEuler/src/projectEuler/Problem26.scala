package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/26/12
  * Time: 2:31 PM
  * Find longest recurring cycle in decimal fraction part
  * == Change Log ==
  * 9/26/12 - initial creation
  * solveed and submitted
  */
object Problem26 {

  def hasMaxRepeatingDigits(p: Int, c: Int): Boolean = p - c == 1

  def cycleLength(p: Int) = {
    val ten = BigInt(10)
    val periods = for {
      c <- 1 to p
      if (ten.modPow(c, p) - 1 == 0)
    } yield c
    periods.min
  }

  def main(args: Array[String]) {
    val justPrimes = primeList(1000).reverse

    val result = justPrimes collectFirst { case x: Int if hasMaxRepeatingDigits(x, cycleLength(x)) => x }
    println("result = " + result)

  }

}
