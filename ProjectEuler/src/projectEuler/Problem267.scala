package projectEuler

/** Created with IntelliJ IDEA.
 * User: DMAK83
 * Date: 12/6/12
 * Time: 10:01 AM
 *
 * == Change Log == 
 * 12/6/12 - initial creation
 */
object Problem267 {
  def bet(b: Double, f: Double, total: Double, count: Int, rand: Double): Double = {
    if (count < 1) total
    else if (isWin(rand)) {
      val newTotes = 2 * b + total
      bet(f * newTotes, f, newTotes, count-1, util.Random.nextGaussian() + 1)
    } else {
      val newTotes = total - b
      bet(f * newTotes, f, newTotes, count-1, util.Random.nextGaussian() + 1)
    }
  }

  def isWin(p: Double): Boolean = -1 <= p && p <= 1

  def invest(f: Double): Double = bet(1.0 * f, f, 1.0, 1000, util.Random.nextGaussian() + 1)

  def main(args: Array[String]) {
    val t = invest(0.18354)
    println("t = " + t)
  }

}
