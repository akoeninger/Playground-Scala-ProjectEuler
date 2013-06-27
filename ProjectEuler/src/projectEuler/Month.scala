package projectEuler

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 8/21/12
  * Time: 8:38 AM
  *
  * == Change Log ==
  * 8/21/12 - initial creation
  */
sealed abstract class Month(val name: String, year: Int) {
  def numDays: Int
  def startingDay(dayOffset: Int): Int
}

case class January(year: Int) extends Month("January", year) {
  def numDays = 31

  def startingDay(dayOffset: Int) = dayOffset
}

case class February(year: Int) extends Month("February", year) {
  def isCentury(y: Int) = y % 100 == 0

  def startingDay(dayOffset: Int) = (dayOffset + 31) % 7

  def numDays = year match {
    case y if isCentury(y) && y % 400 == 0 => 29
    case y if !isCentury(y) && y % 4 == 0  => 29
    case _                                 => 28
  }
}

case class March(year: Int) extends Month("March", year) {
  def numDays = 31

  def startingDay(dayOffset: Int) = 0
}

case class April(year: Int) extends Month("April", year) {
  def numDays = 30

  def startingDay(dayOffset: Int) = 0
}

case class May(year: Int) extends Month("May", year) {
  def numDays = 31

  def startingDay(dayOffset: Int) = 0
}

case class June(year: Int) extends Month("June", year) {
  def startingDay(dayOffset: Int) = 0

  def numDays = 30
}

case class July(year: Int) extends Month("July", year) {
  def numDays = 31

  def startingDay(dayOffset: Int) = 0
}

case class August(year: Int) extends Month("August", year) {
  def numDays = 31

  def startingDay(dayOffset: Int) = 0
}

case class September(year: Int) extends Month("September", year) {
  def numDays = 30

  def startingDay(dayOffset: Int) = 0
}

case class October(year: Int) extends Month("October", year) {
  def numDays = 31

  def startingDay(dayOffset: Int) = 0
}

case class November(year: Int) extends Month("November", year) {
  def numDays = 30

  def startingDay(dayOffset: Int) = 0
}

case class December(year: Int) extends Month("December", year) {
  def startingDay(dayOffset: Int) = 0

  def numDays = 31
}
