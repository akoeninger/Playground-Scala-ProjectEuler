package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/3/12
  * Time: 9:31 AM
  *
  * == Change Log ==
  * 10/3/12 - initial creation
  * solved and submitted
  */
object Problem53 {
  lazy val factsWithPrefix: Stream[BigInt] = BigInt(0) #:: BigInt(1) #::
    (factsWithPrefix.zipWithIndex.tail map {
      case (f, i) => f * i
    })

  val facts = factsWithPrefix drop 1

  def comb(n: Int, r: Int): BigInt = facts(n) / (facts(r) * facts(n - r))

  def main(args: Array[String]) {
    val r = for {
      n <- 23 to 100
      r <- 1 to n
      if (comb(n, r) > 1000000)
    } yield (comb(n, r))

    r foreach println
    println("r = " + r.length)
  }
}

