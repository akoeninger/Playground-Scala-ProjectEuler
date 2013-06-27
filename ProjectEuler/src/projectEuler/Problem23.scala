package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/21/12
  * Time: 2:41 PM
  *
  * == Change Log ==
  * 8/21/12 - initial creation
  * solved and submitted
  */
object Problem23 {
  def divisors(n: Int) = naturals.take(n / 2) filter (d => n % d == 0)

  def isPerfect(n: Int): Boolean = divisors(n).sum == n

  def isDeficient(n: Int): Boolean = divisors(n).sum < n

  def isAbundant(n: Int): Boolean = divisors(n).sum > n

  def main(args: Array[String]) {
    val abundantNumbers = 0 to 28123 filter (p => divisors(p).sum > p)

    val allSumsOfAbundantNumbers = abundantNumbers.view flatMap { a =>
      abundantNumbers.takeWhile(_ <= (28123 - a)).map(a +)
    }

    val notSumOfAbundant = 0 to 28123 diff allSumsOfAbundantNumbers

    val solution = notSumOfAbundant.sum
    println("solution = " + solution)
  }
}
