package projectEuler

import io.Source

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/29/12
  * Time: 3:19 PM
  * Find the number of triangle words in a file
  * == Change Log ==
  * 8/29/12 - initial creation
  * solved and submitted
  */
object Problem42 {
  lazy val triangleNumbers: Stream[Int] = naturals map { i => triSequence(i) }

  def triSequence(n: Int): Int = n * (n + 1) / 2

  def main(args: Array[String]) {
    val f = fileLines("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\words.txt")
    val words = f flatMap { w => w split "," } map { w => w.substring(1, w.length - 1) }
    val wordValues = words map { s => wordValue(s) }

    val (word, maxValue) = wordValues.maxBy(_._2)

    val numbers = triangleNumbers.takeWhile(_ <= maxValue)

    val triangleWords = wordValues filter { v => numbers contains v._2 }

    println("Number of triangle words: " + triangleWords.size)
    println("Triangle Words: " + triangleWords)
  }

  def fileLines(filePath: String): List[String] = {
    Source.fromFile(filePath).getLines().toList
  }

  def wordValue(w: String): (String, Int) = {
    val letterValues = w map {
      case c if c >= 'A' && c <= 'Z' => c - 'A' + 1
      case c if c >= 'a' && c <= 'z' => c - 'a' + 1
      case _                         => 0
    }
    (w,letterValues.sum)
  }
}
