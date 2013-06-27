package projectEuler.poker

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/7/12
  * Time: 12:57 PM
  *
  * == Change Log ==
  * 9/7/12 - initial creation
  */
case class Card(c: String) extends Ordered[Card] {
  val rank = c.dropRight(1) match {
    case "A" => 14
    case "K" => 13
    case "Q" => 12
    case "J" => 11
    case "T" => 10
    case r => r.toInt
  }
  val suit = c.drop(1)

  override def toString: String = c

  def compare(that: Card): Int = this.rank - that.rank

  def ==(that: Card): Boolean = (this compare that) == 0

}
