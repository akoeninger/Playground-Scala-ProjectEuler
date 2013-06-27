package projectEuler.poker

/** Created with IntelliJ IDEA.
  * User: dmak83
  * Date: 9/11/12
  * Time: 4:13 PM
  *
  * == Change Log ==
  * 9/11/12 - initial creation
  */
sealed abstract class Rankings extends Ordered[Rankings] {
  val high: Card
  val name: String

  def tieBreaker(that: Rankings): Int

  def compare(that: Rankings): Int

  def tieR(t1: Seq[Card], t2: Seq[Card]): Int = t1.max compare t2.max match {
    case r if r == 0 && t1.size == 1 => 0
    case r if r == 0 => tieR(t1 diff Seq(t1.max), t2 diff Seq(t2.max))
    case r => r
  }
}

case class HighCard(cds: Seq[Card]) extends Rankings {
  val high = cds.max
  val name = "HighCard"

  def compare(that: Rankings) = that match {
    case x: HighCard => this.high compare x.high
    case _ => -1
  }

  def tieBreaker(that: Rankings) = that match {
    case x: HighCard => tie(x)
    case _ => 0
  }

  def tie(that: HighCard): Int = tieR(this.cds, that.cds)
}

case class OnePair(cds: Seq[Card]) extends Rankings {
  val high = onePair.max
  val name = "Pair"

  private def tie(that: OnePair): Int = tieR(this.cds diff this.onePair, that.cds diff that.onePair)

  def tieBreaker(that: Rankings) = that match {
    case x: OnePair => tie(x)
    case _ => 0
  }

  def onePair: Seq[Card] = cds flatMap {
    c => cds filter {
      c1 => c.rank == c1.rank && c.suit != c1.suit
    }
  }

  def compare(that: Rankings) = that match {
    case x: HighCard => 1
    case x: OnePair if (this.high compare x.high) == 0 => this.tieBreaker(x)
    case x: OnePair => this.high compare x.high
    case _ => -1
  }
}

case class TwoPair(cds: Seq[Card]) extends Rankings {
  val high = pairs.max
  val name = "TwoPair"

  def tieBreaker(that: Rankings) = that match {
    case x: TwoPair => tie(x)
    case _ => 0
  }

  private def tie(that: TwoPair): Int = {
    val lowPair = this.pairs.min compare that.pairs.min

    if (lowPair == 0) {
      tieR(this.cds diff this.pairs, that.cds diff that.pairs)
    }
    else {
      lowPair
    }
  }

  def pairs = cds flatMap {
    c => cds.filter(c1 => c.rank == c1.rank && c.suit != c1.suit)
  }

  def compare(that: Rankings) = that match {
    case x: HighCard => 1
    case x: OnePair => 1
    case x: TwoPair => this.high compare x.high
    case _ => -1
  }
}

case class ThreeOfAKind(cds: Seq[Card]) extends Rankings {
  val high = threeMatches.max
  val name = "Three"

  def tieBreaker(that: Rankings) = that match {
    case x: ThreeOfAKind => tie(x)
    case _ => 0
  }

  private def tie(that: ThreeOfAKind): Int = tieR(this.cds diff this.threeMatches, that.cds diff that.threeMatches)

  def threeMatches = cds flatMap {
    c => cds.filter(c1 => c.rank == c1.rank && c1.suit != c.suit)
  }

  def compare(that: Rankings) = that match {
    case x: HighCard => 1
    case x: OnePair => 1
    case x: TwoPair => 1
    case x: ThreeOfAKind => this.high compare x.high
    case _ => -1
  }
}

case class Straight(cds: Seq[Card]) extends Rankings {
  val high = cds.max
  val name = "Straight"

  private def tie(that: Straight): Int = tieR(this.cds, that.cds)

  def tieBreaker(that: Rankings) = that match {
    case x: Straight => tie(x)
    case _ => 0
  }

  def compare(that: Rankings) = that match {
    case x: HighCard => 1
    case x: OnePair => 1
    case x: TwoPair => 1
    case x: ThreeOfAKind => 1
    case x: Straight => this.high compare x.high
    case _ => -1
  }
}

case class Flush(cds: Seq[Card]) extends Rankings {
  val high = cds.max
  val name = "Flush"

  private def tie(that: Flush): Int = tieR(this.cds, that.cds)

  def tieBreaker(that: Rankings) = that match {
    case x: Flush => tie(x)
    case _ => 0
  }

  def compare(that: Rankings) = that match {
    case x: RoyalFlush => -1
    case x: StraightFlush => -1
    case x: FourOfAKind => -1
    case x: FullHouse => -1
    case x: Flush => this.high compare x.high
    case _ => 1
  }
}

case class FullHouse(cds: Seq[Card]) extends Rankings {
  val high = score.max
  val name = "FullHouse"

  def score = cds collect {
    case c: Card if cds.count(_.rank == c.rank) == 3 => c
  }

  def tieBreaker(that: Rankings) = 0

  def compare(that: Rankings) = that match {
    case x: RoyalFlush => -1
    case x: StraightFlush => -1
    case x: FourOfAKind => -1
    case x: FullHouse => this.high compare x.high
    case _ => 1
  }
}

case class FourOfAKind(cds: Seq[Card]) extends Rankings {
  val name = "Four"
  val high = matches.max

  def tieBreaker(that: Rankings) = 0

  def matches = cds flatMap {
    c => cds.filter(c1 => c.rank == c1.rank && c.suit != c1.suit)
  }

  def compare(that: Rankings) = that match {
    case x: RoyalFlush => -1
    case x: StraightFlush => -1
    case x: FourOfAKind => this.high compare x.high
    case _ => 1
  }
}

case class StraightFlush(cds: Seq[Card]) extends Rankings {
  val name = "StraightFlush"
  val high = cds.max

  private def tie(that: StraightFlush): Int = tieR(this.cds, that.cds)

  def tieBreaker(that: Rankings) = that match {
    case x: StraightFlush => tie(x)
    case _ => 0
  }

  def compare(that: Rankings) = that match {
    case x: RoyalFlush => -1
    case x: StraightFlush => this.high compare x.high
    case _ => 1
  }
}

case class RoyalFlush(cds: Seq[Card]) extends Rankings {
  val high = cds.max
  val name = "Royal"

  def tieBreaker(that: Rankings) = 0

  def compare(that: Rankings) = that match {
    case x: RoyalFlush => tieBreaker(that) // switch to no tie
    case _ => 1
  }
}
