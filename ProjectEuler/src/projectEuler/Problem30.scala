package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/12/12
  * Time: 1:33 PM
 * solved and submitted
  */

object Problem30 {
  val limit = Stream.from(1).find(d => max(d) > sumPowerDigits(max(d), 5)).get

  def max(d: Int) = math.pow(10, d).toInt - 1

  def digitPowered(n: Int, p: Int) = n.toString map { _.asDigit } map { math.pow(_, p) }

  def sumPowerDigits(n: Int, p: Int): Int = digitPowered(n, p).sum

  def main(args: Array[String]) {
    val digits = 2 to max(limit) filter { n => n == sumPowerDigits(n, 5) }
    println("digits = " + digits.sum)

  }
}
