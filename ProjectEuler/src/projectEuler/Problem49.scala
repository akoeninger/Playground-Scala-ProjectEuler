package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/6/12
  * Time: 4:59 PM
  *
  * == Change Log ==
  * 9/6/12 - initial creation
  * solved and submitted
  */
object Problem49 {
  def main(args: Array[String]) {
    val primes4Digits = (1001 until 10000 by 2) filter { i => BigInt(i).isProbablePrime(5) }
    val terms = for {
      p <- primes4Digits
      if (primes4Digits.contains(p + 3330))
      if (primes4Digits.contains(p + 6660))
    } yield (p, p + 3330, p + 6660)

    val perms = terms filter { case (t1, t2, t3) =>
      (t1.toString intersect t2.toString intersect t3.toString).size == 4
    }

    val (r1, r2, r3) = perms.last

    val result = r1.toString + r2.toString + r3.toString

    println("result = " + result)
  }
}
