package projectEuler.sudoku

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/17/12
  * Time: 8:28 AM
  *
  * == Change Log ==
  * 8/17/12 - initial creation
  */

class Board(val cells: Seq[Cell]) {
  val isSolved = cells forall (_.isSolved)
  val unsolvedCells = cells filterNot {
    _.isSolved
  }

  def get(row: Int, col: Int) = cells(9 * row + col)

  def set(row: Int, col: Int, value: Int) = {
    val cell = get(row, col)
    if (!cell.isPossible(value)) {
      val msg = "(%d,%d)=%d is not valid".format(row, col, value)
      throw new IllegalAssignmentException(this, row, col, value)
    }
    val newCells = cells map {
      case c if c == cell => new Cell(row, col, value)
      case c if c sameScopeAs cell => c - value
      case c => c
    }
    new Board(newCells)
  }

  def solve: Option[Board] = {
    if (isSolved) return Some(this)
    val c = unsolvedCells minBy {
      _.possibles.size
    }

    for (v <- c.possibles) {
      val b = set(c.row, c.col, v)
      val s = b.solve
      if (s.isDefined) {
        return s
      }
    }
    None
    //   val solved = for {
    //      v <- c.possibles
    //      s <- set(c.row, c.col, v).solve
    //    } yield s
    //    val t = solved.filter(_.isSolved)
    //    t.headOption
  }

  override def toString = cells.foldLeft("") {
    (s, c) =>
      val x = c.value getOrElse '.'
      val hSpace = c.row % Board.DIM == (Board.DIM - 1)
      val vSpace = c.col % Board.DIM == (Board.DIM - 1)
      val lastInCol = c.col == Board.MAX_IDX
      val e = (hSpace, vSpace, lastInCol) match {
        case (_, true, false) => "  "
        case (true, _, true) => "\n\n"
        case (_, _, true) => "\n"
        case _ => " "
      }
      s + x + e
  }
}


object Board {
  val DIM = 3
  val MAX_IDX = 8 // 9 - 1

  private val SZ = 9 * 9
  // DIM2 * DIM2
  private val COORDS = 0 to (SZ - 1) map {
    i => (i / 9, i % 9)
  }

  val blank: Board = {
    val idxs = 0 to MAX_IDX
    val cells = for {
      row <- idxs
      col <- idxs
    } yield new Cell(row, col)

    new Board(cells)
  }

  def read(s: String): Board = {
    var b = Board.blank
    val values = toVals(s)
    for (((row, col), v) <- COORDS.zip(values)) {
      v foreach {
        value => b = b.set(row, col, value)
      }
    }
    b
  }

  private def isDigit(c: Char): Boolean = Character.digit(c, 10) > 0

  private def toVals(s: String): Seq[Option[Int]] = {
    val s1 = s filter {
      _ != '\n'
    }
    println(s1)
    if (s1.length != SZ) throw new IllegalArgumentException("puzzle must be " + SZ + " cells")
    s1 map {
      c =>
        val d = Character.digit(c, 10)
        if (d > 0) Some(d) else None
    }

  }

}