package projectEuler

import annotation.tailrec

/**
 * solved and submitted
 */

object Problem35 {
  def rotate(n: String): String = n.tail + n.head

  def rotations(n: Int): List[Int] = {
    @tailrec
    def rot(x: String, c: Int, acc: List[String]): List[String] = c match {
      case 1 => x :: acc
      case _ => rot(rotate(x), c - 1, x :: acc)
    }
    rot(n.toString, math.ceil(math.log10(n)).toInt, Nil) map (_.toInt)
  }

  def main(args: Array[String]) {
    val starts = prime.takeWhile(_ < 1000000) filter {
      d => d.toString.matches("[^024568]+")
    }

    val pi = starts.filter(i => rotations(i).forall(x => starts.contains(x))).size + 2
    println("pi = " + pi)
  }
}
