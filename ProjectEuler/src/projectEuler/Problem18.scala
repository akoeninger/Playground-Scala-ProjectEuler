package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/16/12
  * Time: 4:03 PM
  *
  * == Change Log ==
  * 8/16/12 - initial creation
  * solved and submitted
  */

object Problem18 {
  val triangle: List[List[Int]] = List(List(75), List(95, 64), List(17, 47, 82), List(18, 35, 87, 10), List(20, 4, 82, 47, 65),
    List(19, 1, 23, 75, 3, 34),
    List(88, 2, 77, 73, 7, 63, 67),
    List(99, 65, 4, 28, 6, 16, 70, 92),
    List(41, 41, 26, 56, 83, 40, 80, 70, 33),
    List(41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
    List(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
    List(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
    List(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
    List(63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
    List(4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23))


  /**
   * Starts at the bottom of list and takes the last row and the next to last row
   * and maps the best path values from the last row to the one above, the recurses up the tree
   * @param top - top row
   * @param bottom - bottom row
   * @return - new top row that will become the bottom row in next round of recursing.
   */

  @tailrec
  def traverse(top: List[List[Int]], bottom: List[Int]): List[Int] = top match {
    case Nil => bottom
    case _   => traverse(top.init, calculatePath(top.last, bottom))
  }

  def calculatePath(top: List[Int], bottom: List[Int]): List[Int] = {
    val newRow = for {
      i <- top.indices
      left = top(i) + bottom(i)
      right = top(i) + bottom(i + 1)
    } yield (math.max(left, right))
    newRow.toList
  }

  def main(args: Array[String]) {
    val result = traverse(triangle.init, triangle.last)
    println("result = " + result)
  }
}
