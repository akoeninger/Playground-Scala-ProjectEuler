package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 10/8/12
  * Time: 8:37 AM
  *
  * == Change Log ==
  * 10/8/12 - initial creation
  */
object Problem61 {
  val octals = filterStartingDigits(polygonal.octaNumbers.slice(19, 57))
  val heptals = filterStartingDigits(polygonal.heptaNumbers.slice(20, 62))
  val hexals = filterStartingDigits(polygonal.hexaNumbers.slice(22, 69))
  val pentals = filterStartingDigits(polygonal.pentaNumbers.slice(25, 80))
  val squares = filterStartingDigits(polygonal.squareNumbers.slice(31, 98))
  val trials = filterStartingDigits(polygonal.triNumbers.slice(43, 138))

  def filterStartingDigits(st: Stream[Int]): List[Int] = st.filter(i => i % 100 > 9).toList

  def nextCyclicNumber(twoDigits: Int, openPolys: Seq[Int]) = openPolys flatMap {
    case 7 => heptals.filter(i => twoDigits == i / 100).map((7, _))
    case 6 => hexals.filter(i => twoDigits == i / 100).map((6, _))
    case 5 => pentals.filter(i => twoDigits == i / 100).map((5, _))
    case 4 => squares.filter(i => twoDigits == i / 100).map((4, _))
    case 3 => trials.filter(i => twoDigits == i / 100).map((3, _))
  }

  def nextNumber(num: Int) = {
    val frontTwo = num / 100
    val backTwo = num % (frontTwo * 100)


  }

  def findNextNumber(twoDigits: Int, pass: Map[Int, Int], oct: Int): Option[Map[Int, Int]] = {

    def nextNumberR(two: Int, pass: Map[Int,Int]): Boolean = {
      if (pass.size == 6) {
        if (pass(8) / 100 == twoDigits) true
      } else {
        val possibles = nextCyclicNumber(twoDigits, Seq(7,6,5,4,3) diff pass.keySet.toSeq)

      }
      false
    }

    if (pass.size == 6) {
      println("Full iteration " + oct)
      if (pass(8) / 100 == twoDigits) Some(pass)
      else None
    } else {
      println("Poss: " +pass.keySet + " Oct: " + oct)
      val possibles = nextCyclicNumber(twoDigits, Seq(7,6,5,4,3) diff pass.keySet.toSeq)

      val passes: Seq[Option[Map[Int, Int]]] = possibles.map(p => findNextNumber(p._2 % 100, pass + p, oct)).filter {
        case Some(x) => x == 6
        case None => false
      }

      val t = passes.flatten
      if (t.isEmpty) None
      else Option(t.head)

    }
  }

  def main(args: Array[String]) {
   // val r = for(i <- octals) yield findNextNumber(i % 100, Map(8 -> i), i)

    //println("r = " + r)
  }

}
