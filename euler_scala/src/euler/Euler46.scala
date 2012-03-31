package euler

import MathUtils._

object Euler46 {

  val primes = for (n <- intStream(2); if checkPrime(n.longValue())) yield n

  def checkPrime(n: Long) = n > 0 && BigInt(n).isProbablePrime(5)

  def checkForSquare(n: Long) = {
    val list = primes.takeWhile(_ < n)
    list.find(p => isSquare((n - p) / 2)).isDefined
  }

  // 5777
  def main(args: Array[String]) {
    timed {
      val oddComposites = for (n <- intStream(9); if n % 2 == 1 && !BigInt(n).isProbablePrime(5)) yield n
      val list = for (n <- oddComposites; if (checkForSquare(n.longValue()))) yield n
      println(oddComposites.zip(list).dropWhile(p => p._1 == p._2).head._1)
    }
  }
}
