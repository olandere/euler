package euler

import euler.MathUtils._
import annotation.tailrec

object Euler37 {

 // val primes = for (n <- intStream(11); if isPrime(n.longValue())) yield n

  def isPrime(n : Long) = {
    BigInt(n).isProbablePrime(7)
  }

  def nextPrime(s: Stream[Long]) = {
    s.dropWhile(!isPrime(_)).head
  }

  def primes(start: Long): Stream[Long] = Stream.cons(start, primes(nextPrime(intStream(start + 1))))


  @tailrec
  def truncatableLtoR(p : Long): Boolean = {
    if (p.toString.contains("0")) false else {
//    println(p)
    isPrime(p) && (if (p > 10) truncatableLtoR(p.toString.drop(1).toLong) else true)
    }
  }

  @tailrec
  def truncatableRtoL(p : Long): Boolean = {
    if (p.toString.contains("0")) false else {
  //  println(p)
    isPrime(p) && (if (p > 10) truncatableRtoL(p.toString.dropRight(1).toLong) else true)
    }
  }

  // 748317
  def main(args: Array[String]) {
    timed{
     //  val list = for (p <- primes(11); if (truncatableLtoR(p) && truncatableRtoL(p))) yield p
      val list = primes(11).filter(p => truncatableLtoR(p) && truncatableRtoL(p))
       println(list.take(11).sum)
    }
  }
}
