package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/6/12
  * Time: 9:43 AM
 * solved and submitted
  */

object Problem3 {
  def primeFactor(n: Long): Int = primeFactors(n).last

  def primeFactors(n: Long): List[Int] = primeList(math.sqrt(n)).filter(n % _ == 0).toList
}
