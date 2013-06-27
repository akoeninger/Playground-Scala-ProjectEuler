package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 1/16/13
  * Time: 9:12 AM
  *
  * == Change Log ==
  * 1/16/13 - initial creation
  */
object Problem51 {

  // Candidate numbers must end in 1,3,7,9
  // Can't replace lowest digit, only valid for above numbers
  // Assuming that solution will have at least 3 digits
  // Group all primes with x digits
  // filter out primes with no duplicates

  def sameDigits(number: Int, firstPlace: Int, secondPlace: Int): Boolean = number.toString()(firstPlace - 1) == number.toString()(secondPlace - 1)

  def hasDuplicates(n: Int) = n.toString.distinct equals n.toString

  def main(args: Array[String]) {
    val t = primeList(999999).toList
    val thousands = t.filter(i => i > 1000 && i < 10000)
    val dups = thousands filterNot hasDuplicates


    dups foreach println
  }

}
