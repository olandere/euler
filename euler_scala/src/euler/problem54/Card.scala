package euler.problem54

case class Card(val rank: Int, val suit: Suit) extends Ordered[Card] {

  override def toString = {
    ""+rank+" of "+suit
  }
  
  def compare(that: Card) = {
    rank.compare(that.rank)
  }
}

object Card {
  def apply(card: String) = {
    require(card.length() == 2)
    val seq = card.toIndexedSeq
    val rank = seq(0) match {
      case 'T' => 10
      case 'J' => 11
      case 'Q' => 12
      case 'K' => 13
      case 'A' => 14
      case c:Char => c.toString.toInt
    }
    new Card(rank, Suit(seq(1)))
  }
}