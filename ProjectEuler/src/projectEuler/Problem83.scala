package projectEuler

import collection.immutable.HashMap


/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/15/12
  * Time: 11:21 AM
  * non-functional
  * == Change Log ==
  * 10/15/12 - initial creation
  * solved and submitted
  */
object Problem83 {
  val matrix: Array[Array[Int]] = parse
  val minValue: Int = matrix.flatten.min
  var openList: HashMap[(Int, Int), (Int, Int)] = HashMap()
  val closedList: Array[Array[Int]] = Array.fill(matrix.length, matrix.length)(0)

  val g: Array[Array[Int]] = Array.fill(matrix.length, matrix.length)(Int.MaxValue)

  /** parallel matrix that holds a cost estimate for each point based on relative distance from target
    * The farther away from the bottom right corner, the higher the estimate
    */
  val h = (g.indices map {
    i => g(i).indices.map(j => (minValue * (2 * len + 1 - i - j))).toArray
  }).toArray

  def fileLines = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\matrix.txt").getLines()

  def parse = fileLines.map(_.split(",").map(_.toInt)).toArray

  def len = matrix.length - 1

  def aStar(): Int = {
    g(0)(0) = matrix(0)(0)
    openList = openList + ((g(0)(0) + h(0)(0), 0) -> (0, 0))

    while (closedList(len)(len) < 2) {
      val (ci, cj) = openList.min._2
      openList = removeNode((ci, cj), openList) // remove current node
      closedList(ci)(cj) = 2 // flag node as closed
      openList = checkAdjacent((ci, cj), openList) // only search tail of openList
    }

    g(len)(len)
  }




  /** Checks if the point is in bounds of the matrix
    *
    * @param r row coordinate, must be less than the number of rows in matrix
    * @param c column coordinate, must be less than length of rows
    * @return true if row and column are in bounds, false otherwise.
    */
  def inBounds(r: Int, c: Int) = r >= 0 && r <= len && c >= 0 && c <= len

  private def removeNode(n: (Int, Int), open: HashMap[(Int, Int), (Int, Int)]): HashMap[(Int, Int), (Int, Int)] = {
    val keyToRemove = open.find(n == _._2).map(_._1)
    val updatedList: Option[HashMap[(Int, Int), (Int, Int)]] = keyToRemove map {
      k => open - k
    }
    updatedList getOrElse open
  }

  def checkAdjacent(cord: (Int, Int), open: HashMap[(Int, Int), (Int, Int)]): HashMap[(Int, Int), (Int, Int)] = {
    val (ci, cj) = cord
    var o = open

    // Check 4 four nodes around current point
    for (k <- 0 until 4) {
      val (ciNew, cjNew) = k match {
        case 0 => (ci - 1, cj)
        case 1 => (ci + 1, cj)
        case 2 => (ci, cj + 1)
        case 3 => (ci, cj - 1)
      }

      // if point is in bounds and not closed
      if (inBounds(ciNew, cjNew) && closedList(ciNew)(cjNew) < 2) {
        // guess for next point is greater than parent's guess + actual value
        if (g(ciNew)(cjNew) > g(ci)(cj) + matrix(ciNew)(cjNew)) {
          g(ciNew)(cjNew) = g(ci)(cj) + matrix(ciNew)(cjNew) // set g-value to be based off parent

          if (closedList(ciNew)(cjNew) == 1) {  // In open list, visited?
            o = removeNode((ciNew, cjNew), o)
          }

          val iCount = o.count(_._1._1 == g(ciNew)(cjNew) + h(ciNew)(cjNew)) // Count the number of nodes with equivalent heuristic values
          o = o + ((g(ciNew)(cjNew) + h(ciNew)(cjNew), iCount) -> (ciNew, cjNew)) // Add to open
          closedList(ciNew)(cjNew) = 1 // Mark as in open list/visited?
        }
      }
    }
    o // Return updated open list
  }

  def main(args: Array[String]) {

    println(aStar())
  }
}
