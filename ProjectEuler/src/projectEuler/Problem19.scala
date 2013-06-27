package projectEuler

/** Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 8/20/12
  * Time: 8:28 AM
  *
  * == Change Log ==
  * 8/20/12 - initial creation
  * 12/11/12 - solved and submitted
  */
object Problem19 {

  def main(args: Array[String]) {
    val lengths = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    val ls = for (y <- 1900 to 2000; m <- 1 to 12) yield {
      if (m == 2) {
        if (y % 100 == 0 && y % 400 == 0) 29
        else if (y % 4 == 0 && y % 100 != 0) 29
        else lengths(m - 1)
      } else {
        lengths(m - 1)
      }
    }

    val fs = ls.foldLeft(List(1))((ws, l) => (ws.head + (l % 7)) % 7 :: ws)


    val result = fs.dropRight(12).count(_ == 0)
    println("result = " + result)
  }

}


