package projectEuler

import collection.mutable

/** Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 11/26/12
  * Time: 10:10 AM
  *
  * == Change Log ==
  * 11/26/12 - initial creation
  */
object Problem78 {
  def numberPartition(n: Int): Int = {
    def intermediate(k: Int, n: Int): Int =
      if (k > n) 0
      else if (k == n) 1
      else intermediate(k + 1, n) + intermediate(k, n - k)

    val p = for (k <- 1 to n / 2) yield intermediate(k, n - k)
    p.sum
  }

  def pentagonal(n: Long): Long = n * (3*n-1) / 2

  def genPentagonal(n: Long): Long = n match {
    case x if x < 0 => 0L
    case x if x % 2 == 0 => pentagonal(x/2+1)
    case x => pentagonal(-(n/2+1))
  }

  def pentagonalStream: Stream[Long] = Stream.from(0L).map(i => genPentagonal(i.toLong))

  def seeds: Stream[BigInt] = BigInt(290797) #:: BigInt(629527) #:: Stream.cons(seeds.tail.head.modPow(2, 50515093), seeds.tail.map(_.modPow(2,50515093)))

  def main(args: Array[String]) {
    //     val result = naturals.drop(1).find(numberPartition(_) % 1000 == 0)
    //    println(result)

    def p(n: Int) = {
      val pt = mutable.MutableList(1)

      for (n <- 1 to n) {
        var r = 0
        var f = -1
        var i = 0
        pentagonalStream.takeWhile(_ <= n) foreach { k =>
          if (i % 2 == 0) { f = -f }
          r += f * pt(n - k)
          i += 1
        }
        pt += r
      }
      pt.last
    }
    val result = naturals.find(p(_) % 10000 == 0)
    println("result = " + result)
  }


}
