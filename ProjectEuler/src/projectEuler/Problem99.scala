package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 1/31/13
  * Time: 10:46 AM
  *
  * == Change Log ==
  * 1/31/13 - initial creation
  * use logs to estimate and simplfy math
  * solved and submitted
  */
object Problem99 {
  val fileLines = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\base_exp.txt").getLines()

  def parse = for {
    l <- fileLines.toList
    an = l.split(",").map(_.toInt)
    pair = an(1) * math.log10(an(0))
  } yield pair


  def main(args: Array[String]) {
    val solution = parse.zipWithIndex.maxBy(_._1)
    println("Solution: %f Line: %d".format(solution._1, solution._2 + 1))
  }

}
