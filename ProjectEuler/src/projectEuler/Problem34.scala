package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/27/12
  * Time: 4:13 PM
  *
  * == Change Log ==
  * 8/27/12 - initial creation
  * solved and submitted
  */
object Problem34 {

  def factorial(n: Int): Long = {
    @tailrec
    def factR(n: Int, acc: Long): Long = n match {
      case 0 => 1
      case 1 => acc
      case _ => factR(n - 1, n * acc)
    }
    factR(n, 1L)
  }

  def candidate(n: Seq[Int]): Boolean = n.size match {
    case 0 => false
    case 1 => false
    case 2 => false
    case 3 => Seq(7,8,9).forall(i => !(n.contains(i)))
    case _ => true
  }

  def digitFactorialSum(n: Int): Long = {
    val digitList = toDigits(n)
    if (candidate(digitList)) {
      val d = toDigits(n) map factorial
      d.sum
    } else 0L

  }

  def main(args: Array[String]) {
      val s = for {
        n <- 1 to 9999999
        if digitFactorialSum(n) == n
      } yield n

      println(s.sum)
  }

}
