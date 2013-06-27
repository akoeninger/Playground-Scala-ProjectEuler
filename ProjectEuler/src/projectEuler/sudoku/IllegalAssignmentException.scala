package projectEuler.sudoku

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/17/12
  * Time: 10:31 AM
  *
  * == Change Log ==
  * 8/17/12 - initial creation
  */

case class IllegalAssignmentException(board: Board,
                                      row: Int,
                                      col: Int,
                                      value: Int)
  extends RuntimeException {
  override def getMessage = {
    "(%d,%d)=%d is not valid".format(row, col, value)
  }

}
