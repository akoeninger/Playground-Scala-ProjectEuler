package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/19/12
  * Time: 9:35 AM
  *
  * == Change Log ==
  * 10/19/12 - initial creation
  */
object Problem139 {

  def pythagoreanTriples(m: Int, n: Int): (Int, Int, Int) = {
    val a = 2 * m * n
    val b = (m * m) - (n * n)
    val c = (m * m) + (n * n)

    if (a > b) (b, a, c) else (a, b, c)
  }


  def main(args: Array[String]) {
    val t = for {
      x <- 1 until 1000
    } yield {
      val xx = x * x
      var y = x + 1
      var z = y + 1
      while(z <= 1000) {
        val zz = xx + y * y
        while(z * z < zz) {z = z + 1}
        if (z * z == zz && z <= 1000) {
            (x, y, z)
        }
        y = y + 1
      }
    }

    t foreach println
  }

}
