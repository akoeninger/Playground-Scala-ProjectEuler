package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/18/12
  * Time: 9:15 AM
  * finished
  * == Change Log ==
  * 10/18/12 - initial creation
  */
object Problem138 {
  lazy val t: Stream[Int] = for {
    l <- Stream.from(1)
    b <- 1 until l
    b2 = b / 2.0
    h = math.sqrt((l * l) - (b2 * b2))
    if math.abs(h - b) == 1
  } yield l

  lazy val fib: Stream[Long] = {
    def loop(h: Long, n: Long): Stream[Long] = h #:: loop(n, h + n)
    loop(0L, 1L)
  }

  def main(args: Array[String]) {
    val k = (1 to 12) map {
      i => fib(6 * i + 3) / 2
    }

    println(k.sum)
  }

}
