package projectEuler.poker

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/7/12
  * Time: 12:58 PM
  *
  * == Change Log ==
  * 9/7/12 - initial creation
  */
class Hand(cds: Seq[String]) {
  val hand = cds.map(Card(_)).sorted
  val cardValues = hand.map(_.rank)
  val cardSuits = hand.map(_.suit)
  private val ranksWithCount = cardValues.map(r => (r, cardValues.count(_ == r))).distinct

  override def toString: String = "Hand(" + hand.mkString(", ") + ")"

  // Hand ranking methods

  def highCard: Card = hand.maxBy(_.rank)

  def hasPair: Boolean = cardValues.distinct.size == 4

  def onePair = hand collect {
    case c: Card if hand.count(_.rank == c.rank) == 2 => c
  }

  def hasTwoPair: Boolean = {
    val pairs = ranksWithCount.filter(_._2 == 2)
    pairs.size == 2
  }

  def twoPair = hand collect {
    case c: Card if hand.count(_.rank == c.rank) == 2 => c
  }

  def hasThreeOfAKind: Boolean = ranksWithCount.exists(_._2 == 3) && !ranksWithCount.exists(_._2 == 2)

  def threeOfAKind = hand collect {
    case c: Card if hand.count(_.rank == c.rank) == 3 => c
  }

  def hasFourOfAKind: Boolean = ranksWithCount.exists(_._2 == 4)

  def isFlush: Boolean = cardSuits.distinct.size == 1

  def isFullHouse: Boolean = (ranksWithCount.exists(_._2 == 2) && ranksWithCount.exists(_._2 == 3))

  def isStraight: Boolean = (hand zip hand.tail) forall {
    case (c1: Card, c2: Card) => c1.rank + 1 == c2.rank
  }

  def isStraightFlush: Boolean = isStraight && isFlush

  def isRoyalFlush: Boolean = isFlush && Seq("T", "J", "Q", "K", "A").forall(cardValues.contains(_))

  // Scoring Methods
  def score = hand match {
    case x if isRoyalFlush => RoyalFlush(x)
    case x if isStraightFlush => StraightFlush(x)
    case x if hasFourOfAKind => FourOfAKind(x)
    case x if isFullHouse => FullHouse(x)
    case x if isFlush => Flush(x)
    case x if isStraight => Straight(x)
    case x if hasThreeOfAKind => ThreeOfAKind(x)
    case x if hasTwoPair => TwoPair(x)
    case x if hasPair => OnePair(x)
    case x => HighCard(x)
  }

}

