package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/17/12
  * Time: 9:02 AM
  * finished
  * == Change Log ==
  * 10/17/12 - initial creation
  */
object Problem112 {
  def isBouncy(n: Int): Boolean = !isDecreasing(n.toString) && !isIncreasing(n.toString)

  @tailrec
  def isDecreasing(n: String): Boolean = n.toList match {
    case Nil => false
    case h :: Nil => true
    case h :: t if t.head > h => false
    case _ => isDecreasing(n.tail)
  }

  @tailrec
  def isIncreasing(n: String): Boolean = n.toList match {
    case Nil => false
    case h :: Nil => true
    case h :: t if t.head < h => false
    case _ => isIncreasing(n.tail)
  }

  def main(args: Array[String]) {
    var count = 0
    val t = Stream.from(1) takeWhile {
      i => if (isBouncy(i)) {
        count = count + 1
      }
      i * 0.99 >= count
    }

    val tt = t.count(isBouncy)
    println(t.last + " " + tt)
  }
}
