package projectEuler

import annotation.tailrec

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 2/6/13
  * Time: 9:50 AM
  *
  * == Change Log ==
  * 2/6/13 - initial creation
  * blatantly stolen solution
  * solved and submitted
  */
object Problem86 {

  def countSolutions(target: Int): Int = {
    // Loop until c > target
    @tailrec
    def targetSolutions(t: Int, c: Int, len: Int): Int = if (c >= t) len - 1
    else targetSolutions(t, c + solutionsForCuboid(len), len + 1)

    targetSolutions(target, 0, 2)
  }

  // Loop until wh is > 2 * l
  def solutionsForCuboid(sideL: Int): Int = {
    @tailrec
    def numSolution(m: Int, wh: Int, num: Int): Int = if (wh > 2 * m) num
    else {
      val sqrt = math.sqrt(wh * wh + m * m)
      val c = if (sqrt == sqrt.toInt) {
        if (wh <= sideL) wh / 2 else 1 + (m - (wh + 1) / 2)
      } else 0
      numSolution(m, wh + 1, num + c)
    }
    numSolution(sideL, 3, 0)
  }

  def main(args: Array[String]) {
    val st = System.currentTimeMillis()
    val s = countSolutions(1000000)
    val et = System.currentTimeMillis() - st
    println(s + " " + et)
  }

}
