package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/13/12
  * Time: 8:42 AM
  * solved, submitted
  * == Change Log ==
  * 8/13/12 - initial creation
  */

object Problem16 {
  def digitSum(exp: Int): Int = BigInt(2).pow(exp).toString().map(_.asDigit).sum

  def main(args: Array[String]) {
    println(digitSum(1000))
  }

}
