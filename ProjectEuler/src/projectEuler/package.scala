/** Provides solutions to Project Euler problems. Also provides sequences and methods that are
  * commonly used to solve problems, such as prime lists, fibonacci numbers, etc.
  *
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/6/12
  * Time: 1:56 PM
  *
  * == Change Log ==
  * 9/6/12 - initial creation
  */
package object projectEuler {
  //  private lazy val primeSieve: Stream[Int] = 2 #:: 3#:: 5 #:: Stream.from(7) filter { i =>
  //    primeSieve takeWhile { j => j * j <= i } forall(i % _ > 0)
  //  }

  lazy val fibonacci: Stream[Int] = {
    def loop(h: Int, n: Int): Stream[Int] = h #:: loop(n, h + n)
    loop(0, 1)
  }

  lazy val naturals: Stream[Int] = Stream.iterate(1)(_ + 1)

  implicit def double2Int(x: Double): Int = x.toInt

  def pyramidNumber(n: Long): Long = (2 * n * n * n + 3 * n * n + n) / 6

  def sumOfNaturals(n: Int): Int = n * (n + 1) / 2

  def primeIndex(index: Int): Int = prime(index)

  lazy val prime = 2 #:: sieve(3)

//  def sieve(n: Int): Stream[Int] =
//    if (prime takeWhile {
//      p => p * p <= n
//    } exists {
//      n % _ == 0
//    }) sieve(n + 2)
//    else n #:: sieve(n + 2)

  def sieve(n: Int): Stream[Int] =
    if (BigInt(n).isProbablePrime(7)) n #:: sieve(n + 2)
    else sieve(n + 2)

  def primeList(limit: Int): Stream[Int] = prime takeWhile (_ < limit)

  def toDigits(n: Int) = n.toString map {
    _.asDigit
  }

  /** Returns the number of co-prime numbers <= n */
  def phi(n: Int): Double = {
    // Some optimizations could be done depending on what
    val pf = (prime.takeWhile(_ <= n).filter(n % _ == 0).toList map {
      p => (p - 1.0) / p
    }).product
    math.round(n * pf)
  }

  // Continued fractions
  def convergentList(n: Int): List[Int] = {
    def loop(x: Int, num: Int, den: Int): Stream[Int] = den match {
      case 1 if num != 0 => (math.sqrt(x).toInt * 2) #:: Stream.empty[Int]
      case _ => {
        val next = ((math.sqrt(x) + num) / den).toInt
        val d2 = num - den * next
        next #:: loop(x, -d2, (x - (d2 * d2)) / den)
      }
    }
    loop(n, 0, 1).toList
  }
}
