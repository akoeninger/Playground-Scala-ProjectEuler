package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/14/12
  * Time: 3:04 PM
  *
  * == Change Log ==
  * 9/14/12 - initial creation
  * 9/19/12 - finished
  */
object Problem69 {
  def phiRatio(n: Int): Double = n / phi(n)

  def main(args: Array[String]) {
    val ps = 2 to 1000000 map { i => phiRatio(i) }

    println(ps.max)

  }
}
