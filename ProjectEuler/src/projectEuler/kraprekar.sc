/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 4/19/13
  * Time: 10:13 AM
  *
  * == Change Log == 
  * 4/19/13 - initial creation
 */
def sortedPairs(num: Int): (Int, Int) = {
  val base4 = "%04d".format(num)
  val desSort = base4.sorted
  val ascSort = desSort.reverse
  (desSort.toInt, ascSort.toInt)
}

def kaprekarRoutine(num: Int): Int = {
  val (desNum, ascNum) = sortedPairs(num)

  (ascNum - desNum) match {
    case 0 => 0
    case 6174 => 6174
    case n => kaprekarRoutine(n)
  }
}
val t = kaprekarRoutine(3333)
println(t)







