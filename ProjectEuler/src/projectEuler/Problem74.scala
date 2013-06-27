package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/10/12
  * Time: 12:04 PM
  * finished
  * == Change Log ==
  * 10/10/12 - initial creation
  */
object Problem74 {
  val memoF = new Memo[Int, Int](factorial)

  def factorial(n: Int): Int = {
    @tailrec
    def factR(n: Int, r: Int): Int = n match {
      case 0 => r
      case 1 => r
      case _ => factR(n - 1, r * n)
    }
    factR(n, 1)
  }

  def digitFactorialSum(n: Long): Long = n.toString.map(_.asDigit).map(memoF).sum

  def factorialSequence(n: Int): List[Long] = {
    @tailrec
    def seqR(i: Long, seq: List[Long]): List[Long] = {
      if (seq.contains(i)) seq
      else seqR(digitFactorialSum(i), seq ++ List(i))
    }

    seqR(n, List())
  }

  def main(args: Array[String]) {
    val chains = for (i <- 1 until 1000000) yield factorialSequence(i).size

    val chainsOf60 = chains.count(_ == 60)
    println("chainsOf60 = " + chainsOf60)
  }

}
