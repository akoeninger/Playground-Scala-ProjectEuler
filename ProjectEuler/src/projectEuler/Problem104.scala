package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 12/12/12
  * Time: 8:48 AM
  *
  * == Change Log ==
  * 12/12/12 - initial creation
  * solved
  */
object Problem104 {
  val tailcut = 1000000000
  val c1 = 0.20898764024997873
  val c2 = 0.3494850021680094

  def top9(n: Int): Int = {
    val t = n * c1 - c2
    (math.pow(10, t - t.toInt + 8)).toInt
  }

  lazy val lowF: Stream[Int] = {
    def f(h: Int, n: Int): Stream[Int] = h #:: f(n, (h + n) % tailcut)

    f(0, 1)
  }

  val panDigits = (1 to 9).map(_.toString).toList

  lazy val candidates: Stream[(Int, Int)] = lowF.zipWithIndex filter {
    x =>
      val digits = x._1.toString.map(_.toString).toList
      panDigits.forall(d => digits.contains(d))
  }

  def isSolution(x: Int): Boolean = {
    val digits = x.toString.map(_.toString).toList
    panDigits.forall(d => digits.contains(d))
  }

  def main(args: Array[String]) {
    candidates find { i => isSolution(top9(i._2)) } foreach println
  }
}
