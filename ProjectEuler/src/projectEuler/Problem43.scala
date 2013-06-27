package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/30/12
  * Time: 9:06 AM
  *
  * == Change Log ==
  * 8/30/12 - initial creation
  * solved and submitted
  */
object Problem43 {
  lazy val panDigitals = "0123456789".permutations
  val divisors = primeList(19).toList

  def pansProperty(s: String): Boolean = {
    val subs = s.drop(1).sliding(3, 1).toList
    val withDivisors = subs zip divisors
    withDivisors forall { t => t._1.toInt % t._2 == 0 }
  }

  def main(args: Array[String]) {
    val validNumbers = panDigitals filter { s => pansProperty(s) }

    val sumOfNums = validNumbers.foldLeft(0L) { _ + _.toLong }

    println("Sum of special pandigital numbers: %,d".format(sumOfNums))
  }


}
