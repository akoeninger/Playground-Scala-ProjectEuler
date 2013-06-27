package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/28/12
  * Time: 8:27 AM
  *
  * == Change Log ==
  * 8/28/12 - initial creation
  * solved and submitted
  */
object Problem33 {
  def cancelOut(n: Seq[Int], d: Seq[Int]): (Int, Int) =
    if (n.head == d.last) (n.last, d.head)
    else if (n.last == d.head) (n.head, d.last)
    else                       (1, 1)

  def main(args: Array[String]) {
    val fractions = for {
      n <- 10 to 99
      d <-  n to 99
      if (n != d)
      nDigits = toDigits(n)
      dDigits = toDigits(d)
      if (nDigits.last == dDigits.head ||
        nDigits.head == dDigits.last)
      dividend = n.toDouble / d.toDouble
      (reN, reD) = cancelOut(nDigits, dDigits)
      reduceDividend = reN.toDouble / reD.toDouble
      if (reduceDividend == dividend)
    } yield (n, d)

    val (nProd, dProd) = fractions reduceLeft { (n, d) => ((n._1 * d._1, n._2 * d._2)) }
    val gcd = BigInt(dProd).gcd(nProd)
    val denom = dProd / gcd

    println("denom = " + denom)
  }
}
