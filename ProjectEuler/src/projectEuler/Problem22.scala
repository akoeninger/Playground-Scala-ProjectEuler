package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/16/12
  * Time: 4:39 PM
  *
  * == Change Log ==
  * 8/16/12 - initial creation
  * solved and submitted
  */

object Problem22 {
  val file = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\names.txt").getLines().toList
  val names = (file.head.split(',') map {
    n => n.substring(1, n.length - 1)
  }).sorted

  def alphaScore(c: Char): Int = (c - 'A') + 1

  def nameScore(n: String): Int = n.map(alphaScore(_)).sum * (names.indexOf(n) + 1)

  val totalNameScores = names.map(nameScore(_)).sum
  def main(args: Array[String]) {
    println("totalNameScores = " + totalNameScores)
  }

}
