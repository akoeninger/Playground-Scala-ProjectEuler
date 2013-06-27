package projectEuler
import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/28/12
  * Time: 11:19 AM
  *
  * Pan digital numbers
  *
  * == Change Log ==
  * 8/28/12 - initial creation
  * solved and submitted
  */
object Problem38 {

  def concatProduct(n: Int): String = {
    @tailrec
    def concatR(n: Int, c: Int, pan: String): String =
      if (pan.length >= 9) pan
      else concatR(n, c + 1, pan + (n * c))
    val r = concatR(n, 1, "")
    if (r.length == 9) r else ""
  }

  def main(args: Array[String]) {
    val concatProducts =  for {
      i <- 9876 to 9123 by -1
      p = i.toString + (2 * i).toString
    } yield p

    val r = concatProducts filter { s => (1 to 9).forall(i => s contains i.toString) }
    println("concatProducts = " + r)
  }
}
