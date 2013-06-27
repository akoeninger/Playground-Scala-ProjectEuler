package projectEuler


/** Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 8/9/12
  * Time: 9:30 AM
  *
  * == Change Log ==
  * 8/9/12 - initial creation
  */

object Problem9 {
  def pythagoreanTriangles(n: Int) = for {
    x <- 1 to n
    y <- x to n
    z <- y to n
    if x * x + y * y == z * z
  } yield (x, y, z)


  def main(args: Array[String]) {
    val triples = pythagoreanTriangles(700)

    val solution = triples filter {
      t => t._1 + t._2 + t._3 == 1000
    }

    solution foreach {
      t => println("T:" + t + " Product: " + t._1 * t._2 * t._3)
    }
  }
}
