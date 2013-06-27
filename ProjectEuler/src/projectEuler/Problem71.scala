package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 11/6/12
  * Time: 8:48 AM
  * solved, nonfunctional, submitted
  * == Change Log ==
  * 11/6/12 - initial creation
  */
object Problem71 {

  /**
   * Thoughts / paper-pencil problem reductions:
   * We are looking for the fraction immediately to the left of 3/7
   * with a reduced denominator no greater than 1,000,000.
   * Lower bound: 2/5 as given
   * Upper bound: 3/7 as given
   * need to only search between these values.
   *
   * @param args program arguments
   */
  def main(args: Array[String]) {
    val a = 3L
    val b = 7L
    var r = 0L
    var s = 1L

   for (q <- 1000000 until 2 by -1) {
     val p: Long = (a * q - 1) / b
     if (p * s > r * q) {
       s = q
       r = p
     }
   }

    println(r + "/" + s)
  }

}
