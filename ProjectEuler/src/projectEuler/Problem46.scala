package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/23/12
  * Time: 8:26 AM
  * Smallest odd composite that can't be written as sum of prime and twice a square.
  * == Change Log ==
  * 10/23/12 - initial creation
  * solved and submitted to PE.
  */
object Problem46 {
  //    To find composites that do satisfy the condition
  //    Given a odd, composite integer,
  //    1) get all primes less than num,
  //    2) we want to find a prime from the subset,
  //    3) the diff between the integer and the prime, divided by two, should be a squared number,
  //    4) num-prime = diff, sqrt(diff / 2) => valid int
  //    5) you can always subtract a lesser prime from the integer, so where this will fail is
  //       in not being able to find a valid diff that is twice a square.

  def oddComposite(x: Int): Option[Int] = primeList(x) find {
    p => math.sqrt((x - p) / 2) % 1 == 0
  }

  def main(args: Array[String]) {
    val t = System.currentTimeMillis()
    val smallest = naturals.filter(n => n > 1 && n % 2 != 0 && !BigInt(n).isProbablePrime(5)) find {
      x => oddComposite(x) == None
    }
    val f = System.currentTimeMillis() - t
    println("smallest = " + smallest)
    println("f = " + f)
  }
}
