package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/27/12
  * Time: 4:11 PM
  *
  * Smallest number divisible by each number in a range
  *
  * == Change Log ==
  * 8/27/12 - initial creation
  * solved submitted
  */
object Problem5 {
  val nums = Range(20, Int.MaxValue, 20)
  def smallestDiv: Int =  nums.find(n => Range(2, 21).forall(n % _ == 0)).get

  def main(args: Array[String]) {
    println(smallestDiv)
  }
}
