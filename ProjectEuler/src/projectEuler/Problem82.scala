package projectEuler


/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/12/12
  * Time: 11:15 AM
  *
  * == Change Log ==
  * 10/12/12 - initial creation
  * finished, non functional , submitted
  */
object Problem82 {
  val matrix: Array[Array[Int]] = parse

  def fileLines = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\matrix.txt").getLines()

  def parse = fileLines.map(_.split(",").map(_.toInt)).toArray

  def len = matrix.length - 1

  // holds the minimum aggregate cost for the solution path starting in each row
  val sol: Array[Int] = (for (i <- 0 to len) yield matrix(i)(len)).toArray


  def pathSum() {
    for (i <- len - 1 to 0 by -1) {
      sol(0) += matrix(0)(i) // calculates sum of going straight across the top
      for (j <- 1 to len) {
      // working down col(i), chose the solution path to the right or solution path above
      // update value in sol for row j
        sol(j) = matrix(j)(i) + sol(j).min(sol(j - 1))
      }
      for (j <- len - 1 to 0 by -1) {
        // working up col(i), determine if taking the current solution path at this point is better than
        // the path from the current point to the solution path of row below
        sol(j) = sol(j).min(sol(j + 1) + matrix(j)(i))
      }
    }
  }

  def main(args: Array[String]) {
    println(sol mkString ",")

    pathSum()
    println(sol.min)
    println(sol mkString ",")
  }

}
