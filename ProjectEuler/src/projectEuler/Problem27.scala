package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/28/12
  * Time: 8:32 AM
  *
  * Considering quadratics of the form:
  * n² + an + b, where |a| < 1000 and |b| < 1000
  * Find the product of the coefficients, a and b,
  * for the quadratic expression that produces the
  * maximum number of primes for consecutive values of n,
  * starting with n = 0.
  *
  * Observations
  * 1) b must be prime b/c  f(0) = 0 + a(0) + b,
  * 2) -a approaches 0
  * 3) a must be odd: n² + an is always even, even + prime is odd
  * 4) a != 0: n² + an alternates between even / odd
  * 5) |b| != 0, 2, 5
  *
  *
  * == Change Log ==
  * 9/28/12 - initial creation
  * solved and submitted
  */
object Problem27 {
  val A = naturals.take(999).filter(x => x % 2 != 0).toList
  val B = primeList(1000).drop(2).toList

  def quad(n: Int, a: Int, b: Int): Int = (n * n) + (a * n) + b

  def numPrimes(a: Int, b: Int): Int = {
    val c = Stream.from(0).takeWhile(n => BigInt(quad(n, a, b)).isProbablePrime(4))
    c.size
  }

  def main(args: Array[String]) {
     val allPos = for {
       a <- A
       b <- B
     } yield (a,b,numPrimes(a,b))

    val negAPosB = for {
      a <- A
      b <- B
    } yield (-a,b,numPrimes(-a,b))

    val negBPosA = for {
      a <- A
      b <- B
    } yield (a,-b,numPrimes(a,-b))

    val allNeg = for {
      a <- A
      b <- B
    } yield (-a,-b,numPrimes(-a,-b))

    val maxAllPos = allPos.maxBy(_._3)
    val maxNegAPosB = negAPosB.maxBy(_._3)
    val maxNegBPosA = negBPosA.maxBy(_._3)
    val maxAllNeg = allNeg.maxBy(_._3)

    val (a, b, num) = Set(maxAllPos,maxNegAPosB,maxNegBPosA,maxAllNeg).maxBy(_._3)
    println("a = %d b = %d num = %d".format(a,b,num))
   val result = a * b

    println("result = " + result)
  }
}
