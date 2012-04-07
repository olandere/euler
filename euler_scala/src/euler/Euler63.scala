package euler

import euler.MathUtils._

object Euler63 {

  def func(n: Int) = {
   // intStream(2).map(BigInt(_).pow(n)).filter(_.toString.length == n).takeWhile(_ < BigInt(10).pow(n)).toList
    intStream(1).map(x => (x, BigInt(x).pow(n))).dropWhile(_._2.toString.length < n).takeWhile(_._2.toString.length == n).toList
//    intStream(1).map(BigInt(_).pow(n)).dropWhile(_.toString.length < n).takeWhile(_.toString.length == n).toList
  }

  //49
  def main(args: Array[String]) {
    timed {
      val nums = for (val n <- intStream(1)) yield func(n.toInt)
      val result = nums.takeWhile(_.length > 0).flatten
     println(result.toList)
      println(result.length)
    }
  }
}
