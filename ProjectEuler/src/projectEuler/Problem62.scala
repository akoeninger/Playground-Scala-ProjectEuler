package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/9/12
  * Time: 8:31 AM
  *
  * == Change Log ==
  * 10/9/12 - initial creation
  * 11/15/12 - solved submitted
  */
object Problem62 {
  def lBound(d: Int) = math.ceil(math.cbrt(math.pow(10, d - 1))).toInt

  def uBound(d: Int) = math.cbrt(math.pow(10, d)).toInt

  def digits(n: Int): Seq[(Int, Long)] = for (d <- lBound(n) to uBound(n)) yield (d, math.pow(d, 3).toLong)


  def main(args: Array[String]) {

    val t = digits(12)
    //    val perms = t.map(i => t.filter(s => (i._2.toString diff s._2.toString) == ""))
    val perms = t find {
      i => t.filter(s => (i._2.toString diff s._2.toString) == "").size == 5
    }

    println("p = " + perms)
  }

}
