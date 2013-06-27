package projectEuler

/** Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 10/25/12
  * Time: 10:21 AM
  * solved and submitted
  * == Change Log ==
  * 10/25/12 - initial creation
  */
object Problem65 {
  lazy val eStream: Stream[BigInt] = {
    // convergents of continued fraction of e
    def loop(k: Int): Stream[BigInt] = 1 #:: (2 * k) #:: 1 #:: loop(k + 1)
    2 #:: loop(1)
  }

  def main(args: Array[String]) {
    lazy val h: Stream[BigInt] = eStream.zipWithIndex map {
      case (a, 0) => a
      case (a, 1) => (a * h(0)) + 1
      case (a, n) => (a * h(n - 1)) + h(n - 2)
    }

    val sum = h(99).toString().map(_.asDigit).sum
    println("sum = " + sum)

  }

}
