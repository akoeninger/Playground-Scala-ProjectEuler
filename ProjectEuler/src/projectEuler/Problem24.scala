package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/23/12
  * Time: 3:45 PM
  *
  * == Change Log ==
  * 8/23/12 - initial creation
  * solved and submitted
  */
object Problem24 {
  def lexicographicPerm(s: String, p: Int) = s.toList.permutations.toStream(p)

  def main(args: Array[String]) {

    println("Result: " + lexicographicPerm("0123456789", 999999))
  }

}
