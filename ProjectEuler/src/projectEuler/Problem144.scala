package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 12/17/12
  * Time: 9:48 AM
  *
  * == Change Log ==
  * 12/17/12 - initial creation
  * solved and submitted
  */
object Problem144 {

  def deriv(x: Double, y: Double) = -4 * x / y

  def slope(p1: (Double, Double), p2: (Double, Double)) = (p2._2 - p1._2) / (p2._1 - p1._1)

  def quadratic(m2: Double, n: Double, x2: Double): Double = {
    val a = 4 + m2 * m2
    val b = 2 * m2 * n
    val c = n * n - 100
    val ans1 = (-b + math.sqrt(b * b - 4 * a * c)) / (2 * a)
    val ans2 = (-b - math.sqrt(b * b - 4 * a * c)) / (2 * a)

    val dx1 = if ((x2 - ans1) > 0) x2 - ans1 else -(x2 - ans1)
    val dx2 = if ((x2 - ans2) > 0) x2 - ans2 else -(x2 - ans2)

    if (dx1 > dx2) ans1 else ans2
  }

  def nextPoint(p1: (Double, Double), p2: (Double, Double)): (Double, Double) = {
    val incomingSlope = slope(p1, p2)
    val tangentSlope = deriv(p2._1, p2._2)
    val X = (incomingSlope - tangentSlope) / (1 + incomingSlope * tangentSlope)
    val refSlope = (tangentSlope - X) / (1 + X * tangentSlope)
    val b = p2._2 - refSlope * p2._1

    val x2 = quadratic(refSlope, b, p2._1)
    val y2 = refSlope * x2 + b

    // println(x2 + ", " + y2)
    (x2, y2)
  }

  def exitPoint(p: (Double, Double)) = p._2 > 0 && p._1 > -0.01 && p._1 < 0.01

  def main(args: Array[String]) {
    lazy val reflections: Stream[(Double, Double)] = (0.0, 10.1) #::(1.4, -9.6) #:: reflections.zip(reflections.tail).map(p => nextPoint(p._1, p._2))

    val results = reflections.drop(1).zipWithIndex.find(r => exitPoint(r._1))
    results foreach println
  }

}
