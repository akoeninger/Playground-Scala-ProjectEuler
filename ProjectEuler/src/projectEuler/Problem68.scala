package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/10/12
  * Time: 8:41 AM
  * finished, submitted
  * == Change Log ==
  * 10/10/12 - initial creation
  */
object Problem68 {

  def isInternal(e: Int, r: Array[Int]): Boolean = {
    val index = r.indexOf(e)
    index % 2 == 0 && index != 0 || index == 1
  }

  def invalidStart(r: Array[Int]): Boolean = r(3) < r(0) ||
    r(5) < r(0) ||
    r(7) < r(0) ||
    r(9) < r(0)

  def validSolution(p: Array[Int]): Boolean = {
    val s = p(0) + p(1) + p(2)
    if (s != p(3) + p(2) + p(4)) return false
    if (s != p(5) + p(4) + p(6)) return false
    if (s != p(7) + p(6) + p(8)) return false
    if (s != p(9) + p(8) + p(1)) return false

    true
  }

  def isResult(r: Array[Int]): Boolean = {
    !isInternal(10, r) && !invalidStart(r) && validSolution(r)
  }

  def main(args: Array[String]) {
    val solutions = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).permutations filter {
      a => a(0) == 6 && isResult(a)
    }

    val max = solutions map {
      s => "" +
        s(0) + s(1) + s(2) +
        s(3) + s(2) + s(4) +
        s(5) + s(4) + s(6) +
        s(7) + s(6) + s(8) +
        s(9) + s(8) + s(1)
    }


    println(max.max)
  }
}

