package projectEuler

/** Created with IntelliJ IDEA.
 * User: dmak83
 * Date: 12/18/12
 * Time: 10:04 AM
 *
 * == Change Log == 
 * 12/18/12 - initial creation
 */
object Problem142 {
  val t2 = for {
    x <- 3 to 1000
    y <- 2 until x
    z <- 1 until y
    xmy = x - y
    xpy = x + y
    xmz = x - z
    xpz = x + z
    zmy = y - z
    zpy = y + z
    if math.sqrt(zmy) % 1 == 0
    if math.sqrt(zpy) % 1 == 0
    if math.sqrt(xmy) % 1 == 0
    if math.sqrt(xpy) % 1 == 0
    if math.sqrt(xmz) % 1 == 0
    if math.sqrt(xpz) % 1 == 0
  } yield (x, y, z)

  def main(args: Array[String]) {
    t2 foreach println
  }

}
