package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/9/12
  * Time: 8:43 AM
  * finished
  * == Change Log ==
  * 10/9/12 - initial creation
  * 12/10/12 - improved, made functional, submitted
  */
object Problem67 {

  @tailrec
  def path(top: Array[Array[Int]], bottom: Array[Int]): Array[Int] = top match {
    case x if x.length == 0 => bottom
    case _                  => path(top.init, calculatePath(top.last, bottom))
  }

  def calculatePath(top: Array[Int], bottom: Array[Int]): Array[Int] = {
    val newRow = for {
      i <- top.indices
      left = top(i) + bottom(i)
      right = top(i) + bottom(i + 1)
    } yield (math.max(left, right))
    newRow.toArray
  }

  def main(args: Array[String]) {
    val t = System.currentTimeMillis()
    val rows: Array[String] = scala.io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\triangle.txt").getLines().toArray
    val rowArrays: Array[Array[Int]] = rows map {
      string => string.split(" ") map {
        _.toInt
      }
    }

    //    for (i <- rowArrays.length - 2 to -1 by -1; j <- 0 until i + 1) {
    //      println(j)
    //      rowArrays(i)(j) += math.max(rowArrays(i + 1)(j), rowArrays(i + 1)(j + 1))
    //    }
    //
    val result = path(rowArrays.init, rowArrays.last)
    val elapsed = System.currentTimeMillis() - t
    println(result(0) + " " + elapsed)


  }

}
