package euler

import Rational._
import collection.mutable.{ListBuffer, Buffer}

object Euler57 {

  val b0 = 1

  def modifiedLentz = {
    val result: Buffer[Rational] = new ListBuffer
    var f: Rational = b0
    var c: Rational = f
    var d: Rational = 0
    for (j <- 1 to 1000) {
      d = 2 + d
      c = 2 + 1 / c
      //   println("d: " + d + ", c: " + c)
      d = 1 / d
      val delta = c * d
      f = f * delta
      if (f.numeratorBigger) result += f
      //    println("d: " + d + ", delta: " + delta + ", f: " + f)
    }
    result
  }

  def main(args: Array[String]) {
    println((Rational(3, 15) + Rational(3)))
    println((Rational(6, 12) / 2))
    println(modifiedLentz.length)
    //153
  }
}