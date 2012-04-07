package euler

import MathUtils._

object Euler49 {
  def containsZero(n: Long) = n.toString.contains("0")

  def isPermutation(x: Long, y: Long): Boolean = {
    val xs = x.toString
    val ys = y.toString
    if (!ys.contains(xs(0))) {
      false
    } else {
      if (xs.length == 1 && ys.length == 1) {
        true
      } else {
        isPermutation(xs.substring(1).toLong, ys.replaceFirst(xs(0).toString, "").toLong)
      }
    }
  }

  // assumes a 3 element sorted list
  def isArithmetic(list : List[Long]) = {
    list(1) - list(0) == list(2) - list(1)
  }

  val primes = for (n <- intStream(999); if checkPrime(n.longValue())) yield n

  def checkPrime(n: Long) = BigInt(n).isProbablePrime(5) && !containsZero(n)

  //could be better
  def findSeq1(list: List[Long]): List[Long] = {
    if (list.length < 3) List() else {
    val diff = list.tail.head - list.head
    val diff1 = list.tail.tail.head - list.tail.head
    (diff-diff1) match {
      case 0 => list.take(3)
      case n if n < 0 => findSeq1(list.head :: list.tail.drop(1)) ++ findSeq1(list.tail)
      case _ => findSeq1(list.take(2) ++ list.drop(3)) ++ findSeq1(list.tail)
    }
    }
  }

  //2969, 6299, 9629
  def main(args: Array[String]) {
    timed {
      val list = primes.takeWhile(_ < 10000).toList
  //    println (list.length)
    //  println(list)
    //  println(list.filter(isPermutation(list.head, _)) )

      val l2 = (for(x <- list; val y = list.filter(isPermutation(x, _)); if (y.length >= 3)) yield y).distinct
      //println(l2.filter(isArithmetic))
    //  println(l2)
      println(l2.map(findSeq1).filter(_.length>0))
    }
  }
}
