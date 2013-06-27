package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 12/14/12
  * Time: 9:14 AM
  *
  * == Change Log ==
  * 12/14/12 - initial creation
  * solved and submitted, solved by hand first
  */
object Problem79 {
  val file = io.Source.fromFile("C:\\Development\\ProjectEuler\\ProjectEuler\\src\\resources\\keylog.txt")
  val attempts = file.getLines().toList


  val firstPosFreq = {
    val firsts = for (a <- attempts) yield a.head
    firsts.distinct.map(z => (z.toString, firsts.count(_ == z))).sortWith(_._2 >= _._2)
  }

  val secondPosFreq = {
    val second = for (a <- attempts) yield a.drop(1).head
    second.distinct.map(z => (z.toString, second.count(_ == z))).sortWith(_._2 >= _._2)
  }

  val thirdPosFreq = {
    val third = for (a <- attempts) yield a.drop(2).head
    third.distinct.map(z => (z.toString, third.count(_ == z))).sortWith(_._2 >= _._2)
  }

  val startingGuess = (firstPosFreq.take(3) ++ secondPosFreq.take(3) ++ thirdPosFreq.take(3)).map(_._1).distinct

  def checkIndex(in: String, guess: List[String]): Boolean = guess.indexOf(in.head.toString) < guess.indexOf(in.tail.head.toString)

  @tailrec
  def updateCode(at: String, code: List[String]): List[String] = if (at.length <= 1) {
    code
  } else if (checkIndex(at, code)) {
    updateCode(at.drop(1), code)
  } else {
    val temp = code.toArray
    val t1 = (at.head.toString, code.indexOf(at.head.toString))
    val t2 = (at.tail.head.toString, code.indexOf(at.tail.head.toString))

    temp(t1._2) = t2._1
    temp(t2._2) = t1._1
    updateCode(at.tail, temp.toList)
  }

  @tailrec
  def checkOrder(attempt: String, code: List[String]): Boolean = if (attempt.length <= 1) {
    true
  } else if (checkIndex(attempt, code)) {
    checkOrder(attempt.tail, code)
  } else false

  @tailrec
  def crackCode(data: List[String], code: List[String]): List[String] = data match {
    case Nil => code
    case d if checkOrder(d.head, code) => crackCode(data.tail, code)
    case _ => {
      val newCode = updateCode(data.head, code)
      crackCode(data.tail, newCode)
    }
  }


  def main(args: Array[String]) {
    println("startingGuess = " + crackCode(attempts, startingGuess))
  }

}
