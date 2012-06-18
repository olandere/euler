package euler

import MathUtils._
import annotation.tailrec

/**
The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
 */
object Euler50 {

  val LIMIT = 1000000

  def sumCount(list: Seq[Int]) = {
    def checkIt(sum: Int, cnt: Int): (Int, Int) = {
      if (MathUtils.isPrime(sum)) {
        (sum, cnt)
      } else {
        (0, 0)
      }
    }
    @tailrec
    def helper(list: Seq[Int], sum: Int, cnt: Int): (Int, Int) = {
      if (list.isEmpty) {
        checkIt(sum, cnt)
      }
      else
      if (sum + list.head > LIMIT) {
        checkIt(sum, cnt)
      } else {
        helper(list.tail, sum + list.head, cnt + 1)
      }
    }

    if (list.isEmpty) {
      (0, 0)
    }
    else {
      helper(list, 0, 0)
    }
  }

  //997651, 543 primes starting with 7
  def main(args: Array[String]) {
    timed {
      val primes = for (i <- 2 to LIMIT / 2; if (MathUtils.isPrime(i))) yield {
        i
      }
      println(primes.tails.map(sumCount).filter(_._2 > 500).toList)
    }
  }

}
