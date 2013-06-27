package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/12/12
  * Time: 12:56 PM
 * solved and submitted
  */

object Problem21 {
  def d(n: Int): Int = (1 until n).filter(n % _ == 0).sum

  def sumAmicable(n: Int): Int = (0 until n).filter(p => d(d(p)) == p && d(p) != p).sum

  def main(args: Array[String]) {
    val r = sumAmicable(10000)
    println("r = " + r)


  }

}
