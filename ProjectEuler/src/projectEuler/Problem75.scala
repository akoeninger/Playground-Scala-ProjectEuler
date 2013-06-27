package projectEuler

/**
 * Created with IntelliJ IDEA.
 * User: DMAK83
 * Date: 8/6/12
 * Time: 11:16 AM
 *
 * == Change Log ==
 * 8/6/12 - initial creation
 * 8/7/12 - initial solution
 * 8/8/12 - refined algorithm for generating triples, solution incorrect
 */

object Problem75 {
  val dubs = naturalDoubles
  val fractions = for (a <- dubs.take(40); b <- dubs.take(40)) yield Fraction(a, b)
  def pairs: Stream[(Fraction, Fraction)] = for (f1 <- fractions; f2 <- fractions if f1 * f2 == 2.0) yield (f1, f2)
  lazy val t: Stream[(Int, Int, Int)] = pairs map (p => twoFractions(p._1, p._2))
  val trips = t takeWhile (p => p._1 + p._2 + p._3 <= 1500000)

  case class Fraction(nom: Double, den: Double) {
    def *(other: Fraction): Double = (nom / den) * (other.nom / other.den)
  }

  def naturalDoubles: Stream[Double] = naturals map { _.toDouble }

  def twoFractions(f1: Fraction, f2: Fraction): (Int, Int, Int) = {
    val aN = f1.nom + (2.0 * f1.den)
    val aD = f1.den
    val bN = f2.nom + (2.0 * f2.den)
    val bD = f2.den

    val a = aN * bD
    val b = aD * bN
    val c = math.sqrt((a * a) + (b * b))
    if (a < b)
      (a.toInt, b.toInt, c.toInt)
    else
      (b.toInt, a.toInt, c.toInt)
  }

  def triangles(L: Int) = {
    val t = trips filter { p => p._1 + p._2 + p._3 == L }
    t.distinct
  }

  def numTriangles(wire: Int) = triangles(wire).size / 2

  def main(args: Array[String]) {
    trips.distinct foreach println
  }
}
