package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/14/12
  * Time: 3:47 PM
  * solved, submitted
  * == Change Log ==
  * 8/14/12 - initial creation
  */

object Problem15 {
  def PascalsTriangle(r: Int, c: Int): Int = (r, c) match {
    case (0, 0)   => 1
    case (0, _)   => 1
    case (_, 0)   => 1
    //case (r1, c1) if r1 == c1=> 1
    case (r1, c1) => println("r: " + r1 + " c: " + c1);((r1 + 1 - c1) / c1) * PascalsTriangle(r, c - 1)
  }

  @tailrec
  def fact(i: BigInt, r: BigInt = BigInt(1)): BigInt = if (i < 1) r else fact(i - 1, r * i)

  @tailrec
  def f(row: Seq[Long], c: Int): Long = {
    var s = 0L
    val next = row map { n => s += n; s }

    val curried = "\"%13d\"".format(_: Long)
    println(next.map(curried))
    if (c == 0) next.last else f(next, c - 1)
  }

  def r(n: Int) = f(List.fill(n + 1)(1L), n - 1)

  def main(args: Array[String]) {
//    val s = fact(40)
//    val d = fact(20)
//    println("d = " + d)
//    println("s = " + s)
//    val result = s / (d * d)
//
//    println("result = " + result)
//
//    r(20)

    val n = fact(40) / (fact(20) * fact(20) )
    println("n = " + n)
  }
}
