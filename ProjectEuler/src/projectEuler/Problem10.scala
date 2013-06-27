package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 7/9/12
  * Time: 3:54 PM
  */

object Problem10 {

  val sum = primeList(2000000).foldLeft(0L)(_ + _)

  def main(args: Array[String]) {
    println("sum = " + sum)
  }
}
