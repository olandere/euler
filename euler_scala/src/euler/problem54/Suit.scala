package euler.problem54

sealed abstract case class Suit(val suit: Char)

case object CLUB extends Suit('C')
case object HEART extends Suit('H')
case object DIAMOND extends Suit('D')
case object SPADE extends Suit('S')

object Suit {
  def apply(suit: Char): Suit = {
    suit match {
      case 'H' => HEART
      case 'C' => CLUB
      case 'D' => DIAMOND
      case 'S' => SPADE
    }
  }
}
