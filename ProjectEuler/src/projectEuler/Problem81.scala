package projectEuler


/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/12/12
  * Time: 10:09 AM
  * finished, non functional, submitted
  * == Change Log ==
  * 10/12/12 - initial creation
  */
object Problem81 {
  val matrix: Array[Array[Int]] = parse

  def fileLines = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\matrix.txt").getLines()

  def parse = fileLines.map(_.split(",").map(_.toInt)).toArray




  /*
I coded in assembler, but it is irrelevant.
 1) Go to the SE corner.
 2) Work vertically up accumulating the values. Back to place.
 3) Work horizontally left accumulating the values. Back to place.
 4) Move NW one step.
 5) Add to this value the E or the S, whichever is smaller.
 6) Work vertically up using the same procedure. Back to place.
 7) Work horizontally left using the same procedure. Back to the place.
 8) Back to (4) unless you just finished row 1
 */

  def other = {
    val len = matrix.length - 1
    val updated = matrix
    val accumulated = for {
      start <- len to 0 by -1 // starts in SE corner, moves NW
    } yield {
       for {
        col <- start to start - 1 by -1
        row <- start to 0 by -1
      } yield {
        (row, col) match {
          case (r, c) if r == start && c == start => {
            if (start == len) updated(r)(c)
            else {
              updated(r)(c) += updated(r + 1)(c).min(updated(r)(c + 1))
              updated(r)(c)
            }
          }
          case (r, c) if c == start => {
            updated(r)(c) += updated(r + 1)(c)
            updated(r)(c)
          }
          case (r, c) if c == start - 1 => {
            if (row == start) updated(start)(row)
            else updated(start)(row) += updated(start)(row + 1)
            updated(start)(row)
          }
        }

      }

    }
    updated
  }

    def pathSum() {
      val len = matrix.length - 1
      for (i <- len to 0 by -1; j <- len to 0 by -1) {
        (i, j) match {
          case (r, c) if r == len && c == len => matrix(r)(c)
          case (r, c) if c == len => matrix(r)(c) += matrix(r + 1)(c)
          case (r, c) if r == len => matrix(r)(c) += matrix(r)(c + 1)
          case (r, c) => matrix(r)(c) += matrix(r + 1)(c).min(matrix(r)(c + 1))
        }
      }
    }

    def main(args: Array[String]) {

      val t = other
      pathSum()
      println("Min sum: " + matrix(0)(0) + " other: " + t(0)(0))
    }

  }
