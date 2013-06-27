package projectEuler

import collection.immutable.Iterable

/** Created with IntelliJ IDEA.
  * User: DMAK83
  * Date: 8/15/12
  * Time: 8:25 AM
  *
  * == Change Log ==
  * 8/15/12 - initial creation
  * solved, submitted
  */

/** Find the total number of letters used if all the numbers from 1 to 1000
  * where written out in words.
  * Ex. 342 = three hundred and forty-two
  *
  */
object Problem17 {
  val singleDigit = Seq("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val decades = Seq("ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  val hundreds = singleDigit.drop(1) map { _ + " hundred" }

  val decadesAndDigits = decades flatMap {
    case "ten" => singleDigit map {
      case ""      => "ten"
      case "one"   => "eleven"
      case "two"   => "twelve"
      case "three" => "thirteen"
      case "five"  => "fifteen"
      case "eight" => "eighteen"
      case s       => s + "teen"
    }
    case d => singleDigit map {
      case "" => d
      case s  => "%s-%s".format(d, s)
    }
  }

  val lowerNumbers = singleDigit ++ decadesAndDigits

  val upperNumbers = hundreds flatMap { h =>
    lowerNumbers map { d => if (d == "") h else "%s and %s".format(h, d) }
  }

  val allNumbers = lowerNumbers.drop(1) ++ upperNumbers ++ Seq("one thousand")

  val numLetters = allNumbers.foldLeft(0)(_ + _.count(i => i != ' ' && i != '-'))

  def main(args: Array[String]) {
    upperNumbers.sliding(10, 10) foreach println
    println("numLetters = " + numLetters)
  }
}
