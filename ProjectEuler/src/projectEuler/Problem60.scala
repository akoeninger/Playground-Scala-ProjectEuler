package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/31/12
  * Time: 9:11 AM
  * == Change Log ==
  * 10/31/12 - initial creation
  * 11/1/12 - solved and submitted
  */
object Problem60 {

  def probablePrime(s: String): Boolean = BigInt(s).isProbablePrime(4)

  def hasProperty(n: Int): Boolean = {
    val first4 = List("3", "7", "109", "673")
    first4.forall(p => probablePrime(p + n) && probablePrime(n + p))
  }

  def conPrimes(n: Int): List[Int] = primeList(30000).filter(x => x > n && (probablePrime(x.toString + n) && probablePrime(n.toString + x))).toList

  def main(args: Array[String]) {
    val basePrimes = primeList(10000)

    val sets = for {
      p1 <- basePrimes
      p1List = conPrimes(p1)
      p2 <- p1List
      p2List = conPrimes(p2)
      p12 = p1List intersect p2List
      p3 <- p12
      p3List = conPrimes(p3)
      p123 = p12 intersect p3List
      p4 <- p123
      p4List = conPrimes(p4)
      p1234 = p123 intersect p4List
      if !p1234.isEmpty
    } yield (p1, p2, p3, p4, p1234.min)

    val solution = sets map {
      p => p._1 + p._2 + p._3 + p._4 + p._5
    }

    println("solution = " + solution.min)
    sets zip solution foreach println
  }

}
