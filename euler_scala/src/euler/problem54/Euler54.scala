package euler.problem54

import scala.io.Source._

object Euler54 {

  def main(args: Array[String]) {
    var (p1wins, p2wins) = (0, 0)
    val lines = fromFile("/Users/eolander/euler/euler_scala/src/euler/problem54/poker.txt").getLines

    lines.foreach {
      l => val (p1, p2) = l.split(" ").map(Card(_)).splitAt(5)
      if (Hand(p1.toList.sortBy(c => c)) > Hand(p2.toList.sortBy(c => c))) p1wins += 1 else p2wins += 1
    }
    println("P1 wins "+p1wins+", P2 wins "+p2wins)

    println(new HighCard(Nil) < new TwoPair(Nil))
    val card = Card("3H")
    println(card.toString)
  }

}