package euler

/*
Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
 */

object Euler27 {
  
  def intStream(a: Int, b: Int): Stream[Int] = Stream.cons(a, intStream(a+b, b))

  // primes less than 1000
  val primes = for (n <- 2 to 1000 ; if checkPrime(n)) yield n

  def checkPrime(n:Int) = {
    n > 0 && BigInt(n).isProbablePrime(10)
  }

  def quad(a: Int, b: Int, n: Int) = {
    n * n + a * n + b
  }

  def seqLength(a: Int, b: Int) = {
    def isOk(n: Int) = {
      checkPrime(quad(a,b,n))
    }
    intStream(0, 1).takeWhile(isOk).length
  }

  // (-61,971,71)
  def main(args: Array[String]) {
    // a must be odd, b must be prime, weed down list by making seq length > 11
    val list = for (a <- intStream(-999, 2).takeWhile(_<1000); b <- primes; val n = seqLength(a,b); if n > 11) yield (a, b, n)
    println (list.toList.sortWith(_._3 > _._3).head)
  }

}
