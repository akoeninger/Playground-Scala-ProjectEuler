package projectEuler


/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/24/12
  * Time: 8:26 AM
  *
  * == Change Log ==
  * 8/24/12 - initial creation
  * 11/1/12 - solved and submitted
  */
object Problem31 {
  val coinSet = Seq(1, 2, 5, 10, 20, 50, 100, 200)

  def calculateWays(ms: Seq[Int], n: Int): Int = ms match {
    case h :: _ if h > n => 0
    case h :: _ if h == n => 1
    case h :: t => calculateWays(ms, n - h) + calculateWays(t, n)
    case _ => 0
  }


  def main(args: Array[String]) {
//    val r = calculateWays(coinSet, 200)
//    println("r = " + r)

    val ways: Array[Int] = Array.ofDim(201)
    ways(0) = 1

    coinSet foreach {
      coin => for (j <- coin to 200) {
        ways(j) += ways(j - coin)
      }
    }

    println(ways(200))
  }
}