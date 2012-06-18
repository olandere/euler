package euler


import scala.math._
import annotation.tailrec
import scala.collection.mutable.Map
import collection.immutable.List

object MathUtils {

  val smallPrimes = 2*3*5*7*11*13

  def intStream(start: Long): Stream[Long] = Stream.cons(start, intStream(start + 1))

  def odds = intStream(1).filter(isOdd)

  def isOdd(n: Long) = (n & 1) == 1

  def isOdd(n: Int) = (n & 1) == 1

  def timed[T](thunk: => T) = {
    val t1 = System.nanoTime
    val ret = thunk
    val time = System.nanoTime - t1
    println("Executed in: " + time / 1000000.0 + " millisec")
    ret
  }

  def divides(a: Long, b: Long): Boolean = {
    b % a == 0
  }

  def factorial(n: BigInt): BigInt = {
    @tailrec
    def facHelper(n: BigInt, acc: BigInt): BigInt = {
      if (n == 1) acc else {
        val t = n - 1
        facHelper(t, acc*t)
      }
    }
    facHelper(n, n)
  }

  // computes (n!)mod m
  def modFac(n: BigInt, m: BigInt):BigInt = {
    def modFacHelper(n: BigInt, m: BigInt, acc: BigInt):BigInt = {
      if (n == 1) acc else {
        val t = n - 1
        modFacHelper(t, m, acc*t % m)
      }
    }
    modFacHelper(n, m, n) % m
  }

  def quadratic(a: Double, b: Double, c: Double) = {
    val q = -0.5 * (b + math.signum(b) * math.sqrt(b * b - 4 * a * c))
    (q / a, c / q)
  }

  def isCongruent(a: BigInt, b: BigInt, n: BigInt): Boolean = {
    (a - b) % n == 0
  }

  def digitalRoot(n: Long) = {
    val r = n % 9
    if (r == 0) 9 else r
  }

  //@tailrec
  final def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }

  final def gcd(x: BigInt, y: BigInt): BigInt = {
    x.gcd(y)
  //  if (x == 0) y
  //  else if (x < 0) gcd(-x, y)
  //  else if (y < 0) -gcd(x, -y)
  //  else gcd(y % x, x)
  }

  final def gcd(x: Long, y: Long): Long = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }

  def factor(n: Long): Seq[Long] = {
    val l = for (x <- 1 to math.sqrt(n).toInt;
                 if (n % x == 0)
    ) yield (x, n / x)
    l.flatMap(a => Seq(a._1, a._2))
  }

//  def primeFactors(n: Long): Seq[Long] = {
//    def primes(nums: Stream[Long]): Stream[Long] =
//        Stream.cons(nums.head,
//          primes((nums tail) filter (x => x % nums.head != 0)))
//
//    
//  }

  def factorExp(f: List[Long]): List[(Long, Int)] = {
    if (f.isEmpty) List() else {
    val t = f.filterNot(_==f.head)
    (f.head, f.length - t.length) :: factorExp(t)
    }
  }

  def numDivisors(n: Long): Int = factorExp(primeFactors(n)).map(x=>x._2+1).product

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

  def isPrime(n : Int) = {
    n match {
      case 2 => true
      case 3 => true
      case 5 => true
      case 7 => true
      case 11 => true
      case 13 => true
      case _ =>  gcd(n, smallPrimes) == 1 && sprp(n, 2) && sprp(n, 3) //&& BigInt(n).isProbablePrime(5)
    }
  }

  def isPrime(n : Long): Boolean = {
    if (n < 20) isPrime(n.toInt)  else
    gcd(n, smallPrimes) == 1 && sprp(n, 2) && sprp(n, 3)//BigInt(n).isProbablePrime(5)
  }

  //Strong probable prime function
  def sprp(n: Long, a: Long) : Boolean = {

    @tailrec
    def decompose(d: Long, s: Long): (Long, Long) = {
      if (isOdd(d))
        (d, s)
      else decompose(d >> 1, s+1)
    }

    @tailrec
    def checkPowerOfTwo(r : Long, s: Long, n: Long): Boolean = {
//      println("r: %d, s: %d, n: %d".format(r,s,n))
      if (s == 0) false else {
        val b = (r*r)%n
        if (b == (n - 1)) true else checkPowerOfTwo(b, s-1, n)
      }
    }

    val(d, s) = decompose(n - 1, 0)
//    println("s: %d, d: %d  %d*2^%s".format(s, d, d, s))
    val r = BigInt(a).modPow(d, n).toLong
    if (r == 1 || r == (n - 1)) true else {
       checkPowerOfTwo(r, s-1, n)
    }
  }

  def totient(n: Int):Int = {
    if (isPrime(n)) {
//      println(""+n+" is prime")
      n - 1
    }else {
      val factors = MathUtils.primeFactors(n)
      if (factors.distinct.length == 1) {
        // this is a power of a prime
        val t = factors.head
//        println(""+n+" is power "+t)

        (n/t*(t-1)).intValue
      } else {
        val dpf = factors.distinct
        val t = dpf.reduce(_*_)
//        println(n+" "+factors+" "+dpf+" "+t)
        ((n/t).intValue)*(dpf.map(_-1).reduce(_*_).intValue)
      }
    }
  }

  def totient(n: Int, factors: List[Long]):Int = {
  //  if (isPrime(n)) n - 1 else {
      if (factors.distinct.length == 1) {
        // this is a power of a prime
        (n*(1-(1.0/factors.head))).intValue
      } else {
        val t = factors.distinct.reduce(_*_)
        (n/t).intValue*factors.distinct.map(_-1).reduce(_*_).intValue
      }
  //  }
  }

  val lastDigitsOfSquare = Set(0L, 1L, 4L, 9L) ++ Set(16L, 21L, 24L, 25L) ++ Set(29L, 36L, 41L, 44L) ++ Set(49L, 56L, 61L, 64L) ++ Set(69L, 76L, 81L, 84L) ++ Set(89L, 96L)

  def isPossibleSquare(n: Long) = {
    //lastDigitsOfSquare(n % 100L)
    (n % 20L) match {
      case 0 => true
      case 1 => true
      case 4 => true
      case 5 => true
      case 9 => true
      case 16 => true
      case _ => false
    }
  //  0, 1, 4, 5, 9, or 16 modulo 20
  }
  
  def longSqrt(n : Long) = {
    math.sqrt(n).toLong
  }

  def isSquare(n: Long) = {
    if (isPossibleSquare(n)) {
      val r = longSqrt(n)
      r * r == n
    } else {
      false
    }
  }

  def factorFermat(n: Long) = {

    @tailrec
    def findXY(x: Long): (Long, Long) = {
      val ysqr = x * x - n
      if (isPossibleSquare(ysqr)) {
        val y = longSqrt(ysqr)
        if (y * y == ysqr) {
          (x, y)
        } else {
          findXY(x + 1)
        }
      } else {
        findXY(x + 1)
      }
    }

    if (n % 2 == 0) {
      (2L, n / 2)
    } else {
      val sqrtn = isqrt(n)
      if (isPossibleSquare(n) && sqrtn * sqrtn == n) {
        (sqrtn, sqrtn)
      } else {
        val (x, y) = findXY(sqrtn + 1)
        (x + y, x - y)
      }
    }
  }

  def primeFactors(n: Long):List[Long] = {
    if (isPrime(n)) List(n) else
    factorFermat(n) match {
      case (x:Long, 1) => List(x)
      case (x:Long, y:Long) => primeFactors(x) ++ primeFactors(y)
    }
  }
  
  def isqrt(n: Long): Long = {

    @tailrec
    def isqrt(n: Long, x: Double): Long = {
      val xn = (x + n / x) / 2
      if (x - xn < 0.5) {
        xn.longValue()
      } else {
        isqrt(n, xn)
      }
    }
    isqrt(n, n)
  }

  //Compute combination nCk -- also binomial coefficient
  def comb(n: Int, k: Int) = {
    var result: Int = 1
    val newK = if (k > n - k) (n-k) else k
    for (j <- 1 to newK) {
      result = (result * (n - (j - 1))) / j
    }
    result
  }

  val partitionCache: Map[(Int, Int), Int] = Map()

  def partition(n: Int): Int = {

    var hits = 0
    var miss = 0
    def p(k: Int, n: Int): Int = {
      if (partitionCache.get(k, n).isDefined) {
       // hits += 1
//        partitionCache.remove((k+1, n))
//        partitionCache = partitionCache - ((k, n-k))
      }
      partitionCache.get(k, n).getOrElse {
      //  miss += 1
        val result = if (k > n) {
          0
        }
        else if (k == n) {
          1
        } else {
          p(k + 1, n) + p(k, n - k)
        }
        partitionCache.put((k, n), result)
        result
      }
    }
    val r = p(1, n)
//    println(partitionCache)
  //  println("hits %d, miss: %d".format(hits, miss))
 //   partitionCache.clear()
 //   hits = 0
 //   miss = 0
    partitionCache.put((1, n), r)
    r
  }

  def nearestSquare(n: Int): Int = {
    @tailrec
    def helper(lo: Int, n: Int): Int = {
      val low = lo * lo
      val hi = (lo + 1) * (lo + 1)
      if (n - low > 0 && hi - n > 0) {
        lo
      } else {
        helper(lo + 1, n)
      }
    }
    helper(0, n)
  }
}