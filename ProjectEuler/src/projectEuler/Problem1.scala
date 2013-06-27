package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 6/21/12
  * Time: 9:11 AM
 * solved and submitted
  */

/** Sum of all natural numbers below n that are multiples of 3 and 5. */
object Problem1 {
  def sum(limit: Int, m1: Int, m2: Int): Int = 1 until limit filter { n =>
    n % m1 == 0 || n % m2 == 0
  } reduceLeft {
    _ + _
  }
}
