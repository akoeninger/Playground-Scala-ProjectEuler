package projectEuler.poker

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/7/12
  * Time: 12:58 PM
  *
  * == Change Log ==
  * 9/7/12 - initial creation
  */
class Player(hands: Seq[Rankings]) {
  def numHighCard: Int = (hands map {
    case x: HighCard => x
    case _ => ""
  }).filterNot(_ == "").size

  def numPair: Int = (hands map {
    case x: OnePair => x
    case _ => ""
  }).filterNot(_ == "").size

  def numTwoPair: Int = (hands map {
    case x: TwoPair => x
    case _ => ""
  }).filterNot(_ == "").size

  def numStraight = (hands map {
    case x: Straight => x
    case _ => ""
  }).filterNot(_ == "").size

  def numThreeKind = (hands map {
    case x: ThreeOfAKind => x
    case _ => ""
  }).filterNot(_ == "").size

  def numFlush = (hands map {
    case x: Flush => x
    case _ => ""
  }).filterNot(_ == "").size

  def numFullHouse = (hands map {
    case x: FullHouse => x
    case _ => ""
  }).filterNot(_ == "").size

  def numFourKind = (hands map {
    case x: FourOfAKind => x
    case _ => ""
  }).filterNot(_ == "").size

  def numStraightFlush = (hands map {
    case x: StraightFlush => x
    case _ => ""
  }).filterNot(_ == "").size

  def numRoyal = (hands map {
    case x: RoyalFlush => x
    case _ => ""
  }).filterNot(_ == "").size

  override def toString = ("HighCards: %d\nPairs: %d\nTwoPair: %d\nThreeKinds: %d\nStraight: %d\n" +
    "FullHouse: %d\nFlush: %d\nFourKind: %d\nStraightFlush: %d\nRoyal: %d").format(
    numHighCard, numPair, numTwoPair, numThreeKind, numStraight,
    numFullHouse, numFlush, numFourKind, numStraightFlush, numRoyal)
}
