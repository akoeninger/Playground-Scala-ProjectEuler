package projectEuler

import poker.{pokerFile, Hand}

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/13/12
  * Time: 4:00 PM
  *
  * == Change Log ==
  * 9/13/12 - initial creation
  * 11/2/12 - solved and submitted, could be cleaned up
  */
object Problem54 {
  def main(args: Array[String]) {
    val pokerHands = {
      val splitLines = pokerFile.getLines().map(s => s.splitAt(s.length / 2)).toList
      val hands = splitLines.map(h => ((h._1.trim split " ").toSeq, (h._2.trim split " ").toSeq))
      hands.map(p => (new Hand(p._1), new Hand(p._2)))
    }

    val handPairs = pokerHands.map(p => (p._1.score, p._2.score))
    val scores = handPairs map {
      case (p1, p2) => if (p1 > p2) 1 else 0
    }

    println(scores.sum)
  }
}
