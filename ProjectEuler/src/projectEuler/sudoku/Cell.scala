package projectEuler.sudoku

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/16/12
  * Time: 2:32 PM
  *
  * == Change Log ==
  * 8/16/12 - initial creation
  */

class Cell(val row: Int, val col: Int, val possibles: Set[Int]) {
  val value = if (possibles.size == 1) possibles.headOption else None
  val isSolved = value.isDefined
  val region = (row / Board.DIM) * Board.DIM + (col / Board.DIM)

  def this(row: Int, col: Int, value: Int) = this(row, col, Set(value))

  def this(row: Int, col: Int) = this(row, col, Cell.ALL_VALUES)

  def -(v: Int) = new Cell(row, col, possibles - v)

  def isPossible(v: Int) = possibles contains v

  def sameScopeAs(c: Cell) = row == c.row || col == c.col || region == c.region

  override def toString = "(%d,%d) %s".format(row, col, possibles.mkString)

}

object Cell {
  val ALL_VALUES = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)

}
