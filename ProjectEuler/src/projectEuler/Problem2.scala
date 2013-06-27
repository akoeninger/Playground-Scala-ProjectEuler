package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/6/12
  * Time: 8:34 AM
 *
 * solved and submitted
  */

object Problem2 {
  // lazy val fib: Stream[Int] = Stream.cons(0, Stream.cons(1, fib.zip(fib.tail).map(p => p._1 + p._2)))
  lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)

  /** Calculates the sum of all even-valued terms of the Fibonacci sequence within a given limit.
    *
    * @param limit the upper bound of the Fibonacci sequence to consider
    * @return Int the sum of the even-valued terms
    */
  def evensSum(limit: Int): Int = fib.takeWhile(_ < limit).filter(_ % 2 == 0).sum
}

