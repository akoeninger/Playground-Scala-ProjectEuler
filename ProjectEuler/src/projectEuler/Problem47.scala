package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/5/12
  * Time: 4:33 PM
  *
  * == Change Log ==
  * 9/5/12 - initial creation
  * distinct prime factors
  * solved and submitted, could use optimization
  *
  */
object Problem47 {

  def factors(n: Int): Seq[Int] = for (i <- 2 to n if n % i == 0) yield i


  def primeFactors(n: Int): Seq[Int] = {
    val possibles = primeList(n).toList

    @tailrec
    def factorize(n: Int, factors: Seq[Int]): Seq[Int] = n match {
      case 1 => factors
      case x if BigInt(x).isProbablePrime(4) => factors :+ x
      case _ => {
        val factor = possibles.find(p => n % p == 0) match {
          case Some(x) => x
          case None => 1
        }

        factorize(n / factor, factors :+ factor)
      }
    }

    factorize(n, Seq())
  }

  def numPrimeFactors(f: Seq[Int]) = {
    val p = primeList(f.max)
    val pf = f filter { n => p contains n }
    pf.size
  }

  def consecutive(start: Int, n: Int) = {
    val ps = start to (start + n - 1) map { factors }
    val fs = ps map { numPrimeFactors }
    fs.forall(_ == n)
  }

  def findConsecutive(n: Int): Seq[Int] = {
    def findR(start: Int, step: Int): Seq[Int] = {
      val r = consecutive(start, step)
      if (r) start to start + step - 1
      else findR(start + 1, step)
    }
    findR(646, n)
  }

  def main(args: Array[String]) {
  //  val s = findConsecutive(2)

    val t = Stream.from(1000).find(i => primeFactors(i).distinct.size == 4 && primeFactors(i+1).distinct.size == 4 && primeFactors(i+2).distinct.size == 4 && primeFactors(i+3).distinct.size == 4)
   // val t = for (i <- 2 to 10700) yield (primeFactors(i).distinct.size)
   // t foreach println
    println("s = " + t)
  }
}
