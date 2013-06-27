package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 1/3/13
  * Time: 10:05 AM
  *
  * == Change Log ==
  * 1/3/13 - initial creation
  * needs more optimizations
  */
object Problem407 {

  def optModM(n: Int) = n match {
    case x if BigInt(x).isProbablePrime(3) => 1
    case x if squareFree(x) => modM(n)
    case _ => 1
  }

  def modM(n: Int): Int = (n - 1 until 0 by -1).find(a => (a * a) % n == a % n) match {
    case None => 0
    case Some(x) => x
  }

  def squareFree(n: Int): Boolean = {
    val factors = primeList(n / 2 + 1).filter(p => n % p == 0)
    factors.forall(p => (n / p) % p != 0)
  }

  def main(args: Array[String]) {
    //    val sol = for {
    //      n <- math.pow(10, 7).toInt to 1 by -1
    //      //if squareFree(n)
    //    } yield (modM(n))
    //
    //    println(sol.sum + " " + sol.size)

    val t = primeList(math.pow(10, 7).toInt).toList

    println(t.size)
  }

}
