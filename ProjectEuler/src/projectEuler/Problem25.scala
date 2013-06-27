package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/27/12
  * Time: 4:11 PM
  *
  * == Change Log ==
  * 8/27/12 - initial creation
  * solved and submitted
  */
object Problem25 {
  lazy val fib: Stream[BigInt] = 0 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)
  def termDigits(n: Int): Int = fib.takeWhile(_.toString().length < n).size

  def main(args: Array[String]) {
    val r: Option[(BigInt, Int)] = fib.zipWithIndex.find(i => i._1.toString().length == 1000)
    println("r = " + r.get._2)
  }
}