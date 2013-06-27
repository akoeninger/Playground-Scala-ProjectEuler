package projectEuler.triangles

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/21/12
  * Time: 8:29 AM
  *
  * == Change Log ==
  * 9/21/12 - initial creation
  */
class Triangle(a: Int, b: Int, c: Int) {
  def perimeter = a + b + c

  val height: Double = {
    val base: Double = c / 2
    math.sqrt((b * b) - (base * base))
  }

  def area = 0.5 * c * height
}

object Tri {
  def triple(u: Int, v: Int): (Int, Int, Int) = {
    val g = u * u
    val h = 2 * v * v
    val i = 2 * u * v

    (g + i, h + i, g + h + i)
  }

  def main(args: Array[String]) {
    val t = for {
      v <- 1 to 1000
      u <- 1 to 1000
      t = triple(u, v)
      if math.abs((2 * t._1) - t._2) == 1 || math.abs((2 * t._2) - t._1) == 1
    } yield (u, v)


    t foreach {
      println
    }
  }
}