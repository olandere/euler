package euler


import scala.math._

object MathUtils {

  def timed[T](thunk: => T) = {
    val t1 = System.nanoTime
    val ret = thunk
    val time = System.nanoTime - t1
    println("Executed in: " + time / 1000000.0 + " millisec")
    ret
  }

  def quadratic(a: Double, b: Double, c: Double) = {
    val q = -0.5 * (b + math.signum(b) * math.sqrt(b * b - 4 * a * c))
    (q / a, c / q)
  }

  def isCongruent(a: BigInt, b: BigInt, n: BigInt): Boolean = {
    (a - b) % n == 0
  }

  //@tailrec
  final def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }

  final def gcd(x: BigInt, y: BigInt): BigInt = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }

  final def gcd(x: Long, y: Long): Long = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }

  def lcm(a: Int, b: Int) = a * b / gcd(a, b)

  def toBase(d: Int, n: Int): Int = {
    if (d < n) d
    else (d % n) + 10 * toBase((d - (d % n)) / n, n)
  }

  def fromBase(n: Long, b: Long): Long = {
    if (n / 10 < 1) n
    else (n % 10) + b * fromBase(n / 10, b)
  }

  def binaryLog(n: Long): Double = {
    log(n) / log(2)
  }
}