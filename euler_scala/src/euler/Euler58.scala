package euler

import MathUtils._

object Euler58 {

  def sw(n: Long) = n * n

  def se(n: Long) = sw(n) - (n - 1)

  def nw(n: Long) = {
    val t = n - 1
    t * t + 1
  }

  def ne(n: Long) = {
    nw(n) - (n - 1)
  }

  def allValues(n: Long) = {
    val t = n - 1
    val r = n * n
    val r1 = r - t
    val r2 = r1 - t
    val r3 = r2 - t
    List(r3, r2, r1)
  }

  def numPrimes(n: Long) = {
    allValues(n).filter(isPrime).length
  }

  def main(args: Array[String]) {
    //  val l = for (n <- odds.tail.takeWhile(_ < 9)) yield numPrimes(n)//(allValues(n), numPrimes(n))//(ne(n), nw(n), se(n), sw(n))
    //  println(l.toList)
    timed {
      var ratio: Float = 1
      var n = 1
      var primes = 0
      while (ratio > 0.1) {
        n = n + 2
        primes = primes + numPrimes(n)
        ratio = primes / ((2 * n - 2) + 1).toFloat

      }
      println("" + n + " " + ratio)
    }
  }
}
