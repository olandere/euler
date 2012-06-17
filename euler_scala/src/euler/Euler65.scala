package euler

import MathUtils._
import Rational._

object Euler65 {

  def eStream(k: Int): Stream[Int] = Stream.cons(1, Stream.cons(2 * k, Stream.cons(1, eStream(k + 1))))

  //272
  def main(args: Array[String]) {
    timed {
            val e = 2 :: eStream(1).take(99).toList
            val r = Rational(e.map(BigInt(_)))
            println(r)
            println(r.n.toString().map(_.toInt - 48).sum)
          }
  }
}
