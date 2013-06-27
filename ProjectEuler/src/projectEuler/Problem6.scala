package projectEuler

import annotation.tailrec

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/2/12
  * Time: 9:02 AM
 * solved and submitted
  */

object Problem6 {
  def square(n: Int): Int = n * n

  def main(args: Array[String]) {
    val sqOfS = squareOfSum(100)
    val suOfS = sumOfSquares(100)

    if (sqOfS < suOfS)
      println("Square of Sum: " + sqOfS)
    else
      println("Sum of Square: " + suOfS)
  }

  def sumOfSquares(n: Int): Int = {
    @tailrec
    def sumR(n: Int, sum: Int): Int = n match {
      case 1 => sum + 1
      case _ => sumR(n - 1, sum + square(n))
    }


    sumR(n, 0)
  }

  def squareOfSum(n: Int): Int = square((1 to n).reduceLeft(_ + _))
}
