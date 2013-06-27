package projectEuler

/** Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 11/20/12
  * Time: 8:24 AM
  * How many ways can 100 be written as a sum of at least 2 integers?
  * == Change Log ==
  * 11/20/12 - initial creation, solved, submitted
  */
object Problem76 {

  def numberPartition(n: Int): Int = {
    def intermediate(k: Int, n: Int): Int =
      if (k > n) 0
      else if (k == n) 1
      else intermediate(k + 1, n) + intermediate(k, n - k)

    val p = for (k <- 1 to n / 2) yield intermediate(k, n - k)
    p.sum
  }

  val solution = numberPartition(100)

  def main(args: Array[String]) {
    println(solution)
  }

}
