package projectEuler

/**
 * Created with IntelliJ IDEA.
 * User: dmak83
 * Date: 7/10/12
 * Time: 8:00 AM
 */

object Problem12 {
  lazy val triNum: Stream[Int] = 0 #:: triNum.zipWithIndex.map(p => p._1 + p._2 + 1)

  def p(t: Int) = Range(1, Int.MaxValue).takeWhile(n => n * n <= t).foldLeft(0)((s, n) => if (t % n == 0) s + 2 else s)

  def numDivisors(d: Int): Int = triNum.find(p(_) > d).get

  def main(args: Array[String]) {
    println(numDivisors(500))
  }
}
