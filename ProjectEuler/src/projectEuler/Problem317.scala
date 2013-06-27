package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 1/4/13
  * Time: 11:02 AM
  *
  * Firecracker - physics
  * == Change Log ==
  * 1/4/13 - initial creation
  * solved and submitted
  */
object Problem317 {

  def main(args: Array[String]) {
    val fragments = for (x <- 0.0 to math.Pi / 2 by 0.0001) yield (new Fragment(x))

    // fragments foreach println

    val verticalFrag = fragments.maxBy(_.maxHeight)
    val horizontalFrag = fragments.maxBy(_.flightDistance)
    val coneHeight = verticalFrag.maxHeight
    val coneRadius = horizontalFrag.flightDistance

    val p = coneHeight / (coneRadius * coneRadius)

    val t = for (x <- (0.000 to coneRadius by 0.0001)) yield (x * fOfX(p, x, coneHeight) * 0.0001)


    // t foreach println
    println("%.4f".format(2.0 * math.Pi * t.sum))
    println(2 * math.Pi * t.sum)


  }

  def fOfX(p: Double, x: Double, q: Double): Double = -p * (x * x) + q

}

class Fragment(angle: Double) {
  val Gravity_Rate = 9.81
  // m / s^2
  val Initial_Velocity = 20.0
  // m / s
  val Initial_Height = 100.0 // m

  val verticalVelocity = if (angle == math.Pi || angle == 0.0) 0.0 else math.sin(angle) * Initial_Velocity
  val horizontalVelocity = if (angle == math.Pi / 2) 0.0 else math.cos(angle) * Initial_Velocity

  def maxHeight = math.pow(verticalVelocity, 2) / (2 * Gravity_Rate) + Initial_Height


  def timeToFreeFall = verticalVelocity / Gravity_Rate

  def freeFallTime = math.sqrt((2 * (maxHeight)) / Gravity_Rate)

  def flightTime = timeToFreeFall + freeFallTime

  def flightDistance = horizontalVelocity * flightTime


  override def toString = "Height: %f Distance: %f".format(maxHeight, flightDistance)
}