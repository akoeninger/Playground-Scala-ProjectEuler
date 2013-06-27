package projectEuler

import util.Random

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 12/5/12
  * Time: 9:37 AM
  *
  * == Change Log ==
  * 12/5/12 - initial creation
  */
class Forest(matrix: Array[Array[Char]]) {
  val TREE = 'T'
  val EMPTY = '.'
  val BURNING = '#'

  val f = 0.001
  val p = 0.1
  val rows = matrix.size
  val cols = matrix(0).size

  def evolve: Forest = {
    val newForest = Array.tabulate(rows, cols) {
      (y, x) =>
        matrix(y)(x) match {
          case EMPTY   => if (Random.nextDouble() < p) TREE else EMPTY
          case BURNING => EMPTY
          case TREE    => if (neighbors(x, y).exists(_ == BURNING)) BURNING
          else if (Random.nextDouble() < f) BURNING
          else TREE
        }
    }
    new Forest(newForest)
  }

  def neighbors(x: Int, y: Int) = matrix.slice(y - 1, y + 2).map(_.slice(x - 1, x + 2)).flatten

  override def toString = matrix.map(_.mkString("")).mkString("\n")

}

object Forest {
  def apply(x: Int = 30, y: Int = 15) = new Forest( Array.tabulate(y, x)((y,x) => if (Random.nextDouble() < 0.5) 'T' else '.'))
}

object ForestFire {
  def main(args: Array[String]) {
    var l = Forest(15,30)
    for (i <- 0 until 50) {
      println(l + "\n-----------------------")
      l = l.evolve
    }
  }
}