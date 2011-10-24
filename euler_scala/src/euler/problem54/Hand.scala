package euler.problem54

sealed abstract class Hand(val cards: List[Card]) extends Ordered[Hand]

case class HighCard(override val cards: List[Card]) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: HighCard => {
        // println("p1: "+this+" p2: "+that);
        this.cards.last.compare(that.cards.last) match {
          case 0 => println("p1: " + this + " p2: " + that); 0
          case i: Int => i
        }
      }
      case _ => -1
    }
  }
}

case class OnePair(override val cards: List[Card], val rank: Int) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: HighCard => 1
      case h: OnePair => {
        this.rank.compare(h.rank) match {
          case 0 => println("p1: " + this + " p2: " + that); this.cards.last.compare(that.cards.last)
          case i: Int => i
        }
      }
      case _ => -1
    }
  }
}

case class TwoPair(override val cards: List[Card]) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: HighCard => 1
      case h: OnePair => 1
      case h: TwoPair => println("p1: " + this + " p2: " + that); 0
      case _ => -1
    }
  }
}

case class ThreeOfAKind(override val cards: List[Card], val rank: Int) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: HighCard => 1
      case h: OnePair => 1
      case h: TwoPair => 1
      case h: ThreeOfAKind => println("p1: " + this + " p2: " + that); this.rank.compare(h.rank)
      case _ => -1
    }
  }
}

case class Straight(override val cards: List[Card]) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: HighCard => 1
      case h: OnePair => 1
      case h: TwoPair => 1
      case h: ThreeOfAKind => 1
      case h: Straight => println("p1: " + this + " p2: " + that); 0
      case _ => -1
    }
  }
}

case class Flush(override val cards: List[Card]) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: RoyalFlush => -1
      case h: StraightFlush => -1
      case h: FourOfAKind => -1
      case h: FullHouse => -1
      case h: Flush => println("p1: " + this + " p2: " + that); 0
      case _ => 1
    }
  }
}

case class FullHouse(override val cards: List[Card]) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: RoyalFlush => -1
      case h: StraightFlush => -1
      case h: FourOfAKind => -1
      case h: FullHouse => println("p1: " + this + " p2: " + that); 0
      case _ => 1
    }
  }
}

case class FourOfAKind(override val cards: List[Card], val rank: Int) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: RoyalFlush => -1
      case h: StraightFlush => -1
      case h: FourOfAKind => println("p1: " + this + " p2: " + that); this.rank.compare(h.rank)
      case _ => 1
    }
  }
}

case class StraightFlush(override val cards: List[Card]) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: RoyalFlush => -1
      case h: StraightFlush => println("p1: " + this + " p2: " + that); 0
      case _ => 1
    }
  }
}

case class RoyalFlush(override val cards: List[Card]) extends Hand(cards) {
  def compare(that: Hand) = {
    that match {
      case h: RoyalFlush => println("p1: " + this + " p2: " + that); 0
      case _ => 1
    }
  }
}

object Hand {
  def isFlush(hand: List[Card]): Boolean = {
    hand.forall(c => c.suit == hand.head.suit)
  }

  def isStraight(hand: List[Card]): Boolean = {
    hand match {
      case h :: Nil => true
      case h :: t => (h.rank + 1 == t.head.rank) && isStraight(t)
    }
  }

  def runLength(hand: List[Card]): Int = {
    hand.takeWhile(c => c.rank == hand.head.rank).size
  }

  def findRuns(hand: List[Card]): Hand = {
    runLength(hand) match {
      case 1 => {
        runLength(hand.tail) match {
          case 4 => new FourOfAKind(hand, hand.tail.head.rank)
          case 3 => new ThreeOfAKind(hand, hand.tail.head.rank)
          case 2 => {
            val newHand = hand.drop(2)
            runLength(newHand) match {
              case 2 => new TwoPair(hand)
              case 1 => new OnePair(hand, newHand.head.rank)
            }
          }
          case 1 => {
            val newHand = hand.drop(2)
            runLength(newHand) match {
              case 3 => new ThreeOfAKind(hand, newHand.head.rank)
              case 2 => new OnePair(hand, newHand.head.rank)
              case 1 => {
                runLength(newHand.tail) match {
                  case 2 => new OnePair(hand, newHand.tail.head.rank)
                  case 1 => new HighCard(hand)
                }
              }
            }
          }
        }
      }
      case 2 => {
        val newHand = hand.drop(2)
        runLength(newHand) match {
          case 3 => new FullHouse(hand)
          case 2 => new TwoPair(hand)
          case 1 => runLength(newHand.drop(1)) match {
            case 1 => new OnePair(hand, hand.head.rank)
            case 2 => new TwoPair(hand)
          }
        }
      }
      case 3 => {
        val newHand = hand.drop(3)
        runLength(newHand) match {
          case 2 => new FullHouse(hand)
          case 1 => new ThreeOfAKind(hand, hand.head.rank)
        }
      }
      case 4 => new FourOfAKind(hand, hand.head.rank)
    }
  }

  def apply(hand: List[Card]): Hand = {
    if (isFlush(hand)) {
      println("Got a flush " + hand)
      if (isStraight(hand)) {
        if (hand.head.rank == 10) {
          println("Got a royal flush " + hand)
          new RoyalFlush(hand)
        } else {
          println("Got a straight flush " + hand)
          new StraightFlush(hand)
        }
      } else
        new Flush(hand)
    } else if (isStraight(hand)) {
      println("Got a straight " + hand)
      new Straight(hand)
    }
    else findRuns(hand)
  }
}