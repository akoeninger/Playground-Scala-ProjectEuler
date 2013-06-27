package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/4/12
  * Time: 9:09 AM
  *
  * == Change Log ==
  * 10/4/12 - initial creation
  * solved and submitted
  */
object Problem58 {
  lazy val sideLengths = Stream.iterate(1)(_ + 2)

  lazy val oddSquares = sideLengths map {
    i => i * i
  }


  def ratio(x: Int): Double = {
    val corners = oddSquares.takeWhile(_ <= x) flatMap {
      i => otherCorners(i)
    }
    println("x= " + x + " corners: " + corners.toList)
    val numPrimes: Double = corners.count(BigInt(_).isProbablePrime(3))

    numPrimes / corners.length
  }

  @tailrec
  def ratioRecursive(cornerStream: Stream[Seq[Int]], pCount: Int, total: Double, sideLength: Int): Int =
    if (pCount / total < 0.1 && sideLength > 1) {
      sideLength
    } else {
      val newPrimes = cornerStream.tail.head.count(i => BigInt(i).isProbablePrime(4))
      ratioRecursive(cornerStream.tail, newPrimes + pCount, total + 4.0, sideLength + 2)
    }

  def otherCorners(x: Int): Seq[Int] = {
    @tailrec
    def computeCorners(c: Int, diff: Int, rem: Int, corners: Seq[Int]): Seq[Int] = rem match {
      case 0 => corners
      case _ => computeCorners(c - diff, diff, rem - 1, corners :+ c)
    }

    val len = math.sqrt(x).toInt
    if (x == 1) {
      Seq(x)
    }
    else {
      computeCorners(x - (len - 1), len - 1, 3, Seq(x))
    }
  }

  def main(args: Array[String]) {
    lazy val corners = oddSquares.map(otherCorners)
    println(ratioRecursive(corners, 0, 1.0, 1))
  }
}
