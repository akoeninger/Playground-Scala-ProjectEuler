package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/5/12
  * Time: 9:34 AM
  *
  * == Change Log ==
  * 10/5/12 - initial creation
  */
package object polygonal {
  lazy val triNumbers: Stream[Int] = (0, 1) #::(1, 3) #:: triNumbers.zip(triNumbers.tail) map {
    n => (n._2 - n._1 + 1) + n._2
  }

  lazy val squareNumbers: Stream[Int] = Stream.from(1) map {
    n => n * n
  }

  lazy val pentaNumbers: Stream[Int] = 1 #:: Stream.from(2) map {
    n => (n * (3 * n - 1)) / 2
  }

  lazy val hexaNumbers: Stream[Int] = 1 #:: Stream.from(2) map {
    n => n * (2 * n - 1)
  }

  lazy val heptaNumbers: Stream[Int] = 1 #:: Stream.from(2) map {
    n => (n * (5 * n - 3)) / 2
  }

  lazy val octaNumbers: Stream[Int] = 1 #:: Stream.from(2) map {
    n => n * (3 * n - 2)
  }


}
