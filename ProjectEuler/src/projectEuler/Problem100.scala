package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 11/30/12
  * Time: 10:47 AM
  * solved, submitted
  * == Change Log ==
  * 11/30/12 - initial creation
  */
object Problem100 {
  def isValid(b: Long, n: Long): Boolean = ((2 * b * b) - (2 * b) - (n * n) + n) == 0

  def nextB(bn: (Long, Long)) = 3 * bn._1 + 2 * bn._2 - 2

  def nextN(bn: (Long, Long)) = 4 * bn._1 + 3 * bn._2 - 3

  lazy val blue: Stream[(Long, Long)] = (15L, 21L) #::(85L, 120L) #:: (blue.tail.map(i => (nextB(i), nextN(i))))

  def main(args: Array[String]) {
    val result = blue.find(i => i._2 >= math.pow(10, 12))
    println("result = " + result)
  }
}
