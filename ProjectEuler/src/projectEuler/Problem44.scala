package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/31/12
  * Time: 11:32 AM
  *
  * == Change Log ==
  * 8/31/12 - initial creation
  * solved and submitted
  */
object Problem44 {
  lazy val pentagonal = Stream.from(0) map { n => n * (3 * n - 1) / 2 }

  def isPentagonal(x: Int) = (math.sqrt(24 * x + 1) % 6) == 5



  def main(args: Array[String]) {
    val sampleSet = pentagonal.take(5000).zipWithIndex.toList.drop(1)

    val sumPairs = for {
      k <- sampleSet
      j <- sampleSet.drop(k._2 + 1)
      sum = k._1 + j._1
      if isPentagonal(sum)
      if isPentagonal((math.abs(k._1 - j._1)))
    } yield (k, j)

    sumPairs foreach println
  }
}
