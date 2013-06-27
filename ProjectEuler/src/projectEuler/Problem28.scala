package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/22/12
  * Time: 8:08 AM
  *
  * == Change Log ==
  * 8/22/12 - initial creation
  * solved and submitted
  */
object Problem28 {
  def diagonals(dim: Int): Seq[Seq[Int]] = {
    var runningTotal = 1
    for (x <- Range(2, dim, 2)) yield {
      for (i <- 1 to 4) yield {
        runningTotal += x
        runningTotal
      }
    }
  }

  def main(args: Array[String]) {
     val solution = diagonals(1001).flatten.sum + 1
    println("solution = " + solution)
  }
}









