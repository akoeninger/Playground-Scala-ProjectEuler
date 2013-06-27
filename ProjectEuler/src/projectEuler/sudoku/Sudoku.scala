package projectEuler.sudoku

import io.Source

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/17/12
  * Time: 10:36 AM
  *
  * == Change Log ==
  * 8/17/12 - initial creation
  */

object Sudoku {
  def main(args: Array[String]) {
    val s = Source.fromFile(args(0)).getLines().mkString
    try {
      val b = Board.read(s)
      println("b = " + b)
      println
      println(b.solve.get)
    } catch {
      case e: IllegalAssignmentException =>
        println(e.board)
        println
        println(e.getMessage)
    }
  }

}
