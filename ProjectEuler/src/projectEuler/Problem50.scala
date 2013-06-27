package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/7/12
  * Time: 8:53 AM
  *
  * == Change Log ==
  * 9/7/12 - initial creation
  * solved and submitted
  */
object Problem50 {
  def approxSumOfPrimes(n: Int) = 0.5 * (n * n) * math.log1p(n)

  def upperBound(x: Int): Int = {
    val ub: Option[Int] = (math.sqrt(x).toInt to 1 by -1).find(i => approxSumOfPrimes(i).toInt < x)
    ub.getOrElse(0)
  }


  def longestSum(n: Int, terms: Seq[Int]): Stream[(Int, Int)] = (for {
  x <- n to 2 by -1
  i <- terms.sliding(x)
    if BigInt(i.sum).isProbablePrime(5)
  } yield(i.sum, x)).toStream

  /*
  hypothesis: the longest sum of primes below n will can be found within first sqrt(n) terms
  from sqrt(n) to 1 find first x where approxSum(x) < n: upper bound of number terms
  get list of first x primes
  start iterating over slides of x groups down
   */
  def main(args: Array[String]) {

    val result = longestSum(561, prime.take(upperBound(1000000)).toList).toList
    val t = result.filter(p => p._1 <= 1000000).maxBy(_._2)
    println("result = " + t)
  }

}
