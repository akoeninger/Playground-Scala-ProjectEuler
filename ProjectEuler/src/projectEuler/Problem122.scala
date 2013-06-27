package projectEuler

import scala.annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 4/29/13
  * Time: 9:45 AM
  *
  * == Change Log ==
  * 4/29/13 - initial creation
  */
object Problem122 {

  val limit = 200
  val path = Array.fill(limit + 1)(0)
  val cost = Array.fill(limit + 1)(Int.MaxValue)

  def traverse(power: Int, depth: Int) {
    if (power < limit) {
      path.update(depth, power)
      cost.update(power, depth)
      traverse(power + power, depth + 1)
    } else {
      backtrack(power / 2, depth - 1)
    }
  }

  def backtrack(power: Int, depth: Int) {
    if (power > limit || depth > cost(power)) {
    } else {
      cost.update(power, depth)
      path.update(depth, power)

      for (i <- depth to 0 by -1) {
        backtrack(power + path(i), depth + 1)
      }
    }

  }


  def main(args: Array[String]) {
    backtrack(1, 0)
    val result = cost.drop(1).sum
    println("result = " + result)
  }

}
