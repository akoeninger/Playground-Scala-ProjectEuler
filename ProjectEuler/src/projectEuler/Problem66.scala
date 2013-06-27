package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/21/12
  * Time: 10:40 AM
  * finished and submitted
  * == Change Log ==
  * 9/21/12 - initial creation
  * 10/26/2012 - submitted
  */
object Problem66 {

  def convergents(n: Int, period: Boolean): Stream[Int] = if (period) convergentPeriod(n) else convergentStream(n)

  def convergentPeriod(n: Int): Stream[Int] = {
    def loop(x: Int, num: Int, den: Int): Stream[Int] = den match {
      case 1 if num != 0 => (math.sqrt(x).toInt * 2) #:: Stream.empty[Int]
      case _ => {
        val next = ((math.sqrt(x) + num) / den).toInt
        val d2 = num - den * next
        next #:: loop(x, -d2, (x - (d2 * d2)) / den)
      }
    }
    loop(n, 0, 1)
  }

  def convergentStream(n: Int): Stream[Int] = {
    def loop(x: Int, num: Int, den: Int): Stream[Int] = den match {
      case _ => {
        val next = ((math.sqrt(x) + num) / den).toInt // check the rounding
        val d2 = num - den * next
        next #:: loop(x, -d2, (x - (d2 * d2)) / den)
      }
    }
    loop(n, 0, 1)
  }

  def convergentPairs(x: Int): Stream[(BigInt, BigInt)] = {
    lazy val convergents: Stream[Int] = convergentStream(x)
    lazy val pairs: Stream[(BigInt, BigInt)] = convergents.zipWithIndex map {
      case (a, 0) => (BigInt(a), BigInt(1))
      case (a, 1) => (BigInt(a) * convergents(0) + 1, BigInt(a))
      case (a, n) => ((a * pairs(n - 1)._1) + pairs(n - 2)._1, a * pairs(n - 1)._2 + pairs(n - 2)._2)
    }
    pairs
  }

  def minimal(d: Int): Option[(BigInt, BigInt)] = convergentPairs(d) find {
    case (x, y) => (x * x) - d * (y * y) == 1
  }

  def main(args: Array[String]) {
    val nonsquares = (2 to 1000).filter(math.sqrt(_) % 1 != 0)
    val result = nonsquares.map(minimal).zip(nonsquares).maxBy(_._1.get._1)
    println(result)
  }
}
