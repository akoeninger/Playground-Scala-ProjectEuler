package projectEuler

import annotation.tailrec

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/12/12
  * Time: 8:17 AM
 * solved and submitted, kind of
  */

object Problem14 {
  @tailrec
  def sequence(n: Long, c: Int = 0): Int = if (n == 1) c + 1 else sequence( if (n % 2 == 0) n / 2 else 3 * n + 1, c + 1)

  def main(args: Array[String]) {
    println("longest = " + sequence(837799))
  }
}
