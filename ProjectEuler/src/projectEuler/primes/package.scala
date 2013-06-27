package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/6/12
  * Time: 2:14 PM
  *
  * == Change Log ==
  * 9/6/12 - initial creation
  */
package object primes {
  lazy val primeSieve: Stream[Int] = 2 #:: 3 #:: 5 #:: Stream.from(7).filter(i => primeSieve.takeWhile(j => j * j <= i).forall(i % _ > 0))

  def primeIndex(index: Int): Int = primeSieve(index)
}
