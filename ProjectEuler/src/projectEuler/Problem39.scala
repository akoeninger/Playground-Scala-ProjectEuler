package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/28/12
  * Time: 4:11 PM
  *
  * == Change Log ==
  * 8/28/12 - initial creation
  * solved and submitted
  */
object Problem39 {
  // a^2 + b^2 = (p - a - b)^2
  def pyth(p: Int, a: Int) = p * (p - 2 * a) % (2 * (p - a))

  /* Does something with the triangle perimeter */
  def tri(p: Int) = {
    val matchingTri = for (a <- 2 to p / 4 + 1 if pyth(p, a) == 0) yield (a)
    (p, matchingTri.size)
  }

  def main(args: Array[String]) {
    val triSolutions = for (p <- 100 to 1000 by 2) yield tri(p)
    val result = triSolutions.maxBy(_._2)

    println(result)
  }
}