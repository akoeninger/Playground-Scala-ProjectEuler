package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/13/12
  * Time: 12:09 PM
 * solved and submitted
  */

object Problem29 {
  def distinctTerms(n: Int, m: Int) = {
   val terms = 2 to n flatMap { a =>
     2 to m map (BigInt(a).pow(_))
   }
    terms.distinct.size
  }

  def main(args: Array[String]) {
    val c = distinctTerms(100, 100)
    println("c = " + c)
  }
}
