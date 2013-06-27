package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/17/12
  * Time: 4:42 PM
  *
  * == Change Log ==
  * 8/17/12 - initial creation
  * solved and submitted
  */

object Problem45 {
  lazy val TriangleNumbers: Stream[Int] = Stream.from(985, 2).map(triangle)
  lazy val HexagonalNumbers: Stream[Long] = Stream.from(144).map(n => hexagonal(n.toLong))

  def triangle(n: Int): Int = n * (n + 1) / 2
  def isTriangle(n: Int) = math.sqrt(8 * n + 1) % 2 == 1.0

  def pentagonal(n: Int): Int = n * (3 * n - 1) / 2
  def isPentagonal(n: Long) = math.sqrt(24 * n + 1) % 6 == 5.0

  def hexagonal(n: Long): Long = n * (2 * n - 1)
  def isHexagonal(n: Int) = math.sqrt(8 * n + 1) % 4 == 3.0

//  val s = for {
//    t <- TriangleNumbers
//    if isPentagonal(t) && isHexagonal(t)
//  } yield t

  def main(args: Array[String]) {
    lazy val t = HexagonalNumbers.find(isPentagonal)
    println("s = " + t.toList)
  }
}
