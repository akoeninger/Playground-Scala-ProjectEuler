package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/24/12
  * Time: 3:26 PM
  *
  * == Change Log ==
  * 8/24/12 - initial creation
  * solved and submitted
  */
object Problem32 {
  val pan = "123456789"

  def isPan(n: String): Boolean = if (n.distinct.length != 9) {
    false
  } else {
    n.forall(c => pan.contains(c))
  }

  def main(args: Array[String]) {

    val result = for {
      i <- 2 until 100
      start = if (i > 9) 123 else 1234
      j <- start until (10000 / i + 1)
      if isPan(i.toString + j.toString + (i * j).toString)
    } yield (i * j)

    println("result = " + result.distinct + " sum: " + result.distinct.sum)
  }
}
