package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/21/12
  * Time: 3:27 PM
  *
  * == Change Log ==
  * 9/21/12 - initial creation
  * 10/30/12 - solved and submitted
  */
object Problem64 {
  def convergentPeriod(n: Int): Stream[Int] = {
    def loop(x: Int, num: Int, den: Int): Stream[Int] = den match {
      case 1 if num != 0 => (math.sqrt(x).toInt * 2) #:: Stream.empty[Int]
      case _ => {
        val next = ((math.sqrt(x) + num) / den).toInt
        val d2 = num - den * next
        next #:: loop(x, -d2, (x - (d2 * d2)) / den)
      }
    }
    loop(n, 0, 1)
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis()
    val periods = for {
      n <- 2 to 10000
      if math.sqrt(n) % 1 != 0.0
    } yield convergentPeriod(n).toList

    val oddPeriods = (periods filter {
      p => p.drop(1).size % 2 != 0 || p.drop(1).size == 1
    }).size

    val end = System.currentTimeMillis() - start
    println("oddPeriods = " + oddPeriods)
    println("end = " + end)
  }

}
