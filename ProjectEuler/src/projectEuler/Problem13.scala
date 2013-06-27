package projectEuler

import io.Source

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/23/12
  * Time: 8:17 AM
  * Work out first 10 digits of the sum of 100 50-digit numbers
  * solved, submitted
  * == Change Log ==
  * 8/23/12 - initial creation
  */
object Problem13 {
  val start = System.currentTimeMillis()
  val lines = Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\numbers").getLines().toSeq
  val extraDigitsNeeded = math.ceil(math.log10(lines.length))
  val numbers = for (n <- lines) yield {
    n.take(10 + extraDigitsNeeded).toLong
  }
  val sum = numbers.sum

  def main(args: Array[String]) {
    println("sum = " + sum.toString.take(10))
    val end = System.currentTimeMillis() - start
    println("end = " + end)
  }
}
