package projectEuler

/**
  * Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/6/12
  * Time: 8:39 AM
  *
  * == Change Log ==
  * 8/6/12 - initial creation
  */

object Problem101 {
  def generator(n: Int): Int = {
    1 - n + math.pow(n, 2) - math.pow(n, 3) + math.pow(n, 4) - math.pow(n, 5) + math.pow(n, 6) - math.pow(n, 7) + math.pow(n, 8) - math.pow(n, 9) + math.pow(n, 10)
    math.pow(n, 10) - math.pow(n, 9) + math.pow(n, 8) - math.pow(n, 7) + math.pow(n, 6) - math.pow(n, 5) + math.pow(n, 4) - math.pow(n, 3) + math.pow(n, 2) - n + 1
  }

  def main(args: Array[String]) {
    val k = generator(3)
    println("k = " + k)

  }
}
