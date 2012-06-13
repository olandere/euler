package euler

import Rational._
import MathUtils._
import annotation.tailrec

// 428570/999997
object Euler71 {

  @tailrec
  def leftMost(r: Rational, h: Rational): Rational = {
    val m = r.mediant(h)
    if (m.d > 1000000) r else leftMost(m, h)
  }

  def main(args: Array[String]) {
    val l = Rational(2,5)
    val h = Rational(3,7)
    timed {
        println(leftMost(l,h) )
    }
  }
}
