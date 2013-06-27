package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 11/7/12
  * Time: 9:01 AM
  * solved and submitted
  * == Change Log ==
  * 11/7/12 - initial creation
  */
object Problem70 {
  def isPrimePower(n: Int): Boolean = primeList(n).exists(p => math.log(n) % math.log(p) == 0.0)

  def primePower(n: Int): Option[Int] = primeList(n).find(p => math.log(n) % math.log(p) == 0.0)

  // faster on individual calls, slower in computing many times seek better opts
  def phiOpt(n: Int): Int = n match {
    case p if BigInt(p).isProbablePrime(5) => (p - 1)
    //    case p if isPrimePower(p) => math.round(p * (1 - (1.0 / primePower(p).get)))
    case p if p > 1 && p % 2 == 0 => if ((p / 2) % 2 == 0) 2 * phiOpt(p / 2) else phiOpt(p / 2)
    case _ => phi(n).toInt
  }

  // fastest yet
  def phiMemo(n: Int, f: Int => Int): Int = n match {
    case p if BigInt(p).isProbablePrime(5) => (p - 1)
    //    case p if isPrimePower(p) => math.round(p * (1 - (1.0 / primePower(p).get)))
    case p if p > 1 && p % 2 == 0 => if ((p / 2) % 2 == 0) 2 * f(p / 2) else f(p / 2)
    case _ => phi(n).toInt
  }

  def isPerm(x: Int, y: Int): Boolean = (x.toString diff y.toString) == ""


  def main(args: Array[String]) {
    val p = Memoize1.Y(phiMemo)
    val primes = primeList(5000).filter(_ >= 2000)
    val phis = for {
      x <- 0 until primes.size - 1
      y <- x + 1 until primes.size
      px = primes(x)
      py = primes(y)
      n = px * py
      if n < 10000000
    } yield (n, (px - 1) * (py - 1))

    val perms = phis.filter(t => isPerm(t._1, t._2))
    val min = perms.map(i => (i._1, i._1.toDouble / i._2)).minBy(_._2)
    println("min = " + min)

  }
}
