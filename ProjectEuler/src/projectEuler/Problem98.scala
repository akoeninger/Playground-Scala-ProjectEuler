package projectEuler

import scala.annotation.tailrec


/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 5/3/13
  * Time: 9:54 AM
  *
  * == Change Log ==
  * 5/3/13 - initial creation
  */
object Problem98 {
  val wordFile = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\words.txt").getLines()

  val wordList = (wordFile flatMap {
    case x: String => x.split(',').toList
  }).toList

  def isAnagram(word: String)(other: String) = word.sorted == other.sorted

  def collectAnagramSets(words: List[String]) = {

    @tailrec
    def buildSet(w: List[String], set: Map[String, Option[List[String]]]): Map[String, Option[List[String]]] = w match {
      case Nil => set
      case list =>
        val anagrams = list.tail collect {
          case i if isAnagram(list.head)(i) => i
        } match {
          case Nil => None
          case x: List[String] => Some(x)
        }

        val setMap = set + (list.head -> anagrams)

        buildSet(list.tail, setMap)
    }
   buildSet(words, Map[String, Option[List[String]]]())

  }


  def main(args: Array[String]) {

    val t = collectAnagramSets(wordList)

    t foreach println
  }

}
