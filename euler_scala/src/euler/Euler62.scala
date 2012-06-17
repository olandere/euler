package euler

import MathUtils._
import collection.mutable.{ListBuffer, HashMap}

object Euler62 {

  val zeros = 1
  val ones = zeros << 3
  val twos = ones << 3
  val threes = twos << 3
  val fours = threes << 3
  val fives = fours << 3
  val sixes = fives << 3
  val sevens = sixes << 3
  val eights = sevens << 3
  val nines = eights << 3

  def bitVector(n: Long): Int = {
    if (n == 0) 0 else {
      val r = (n % 10) match {
        case 0 => zeros
        case 1 => ones
        case 2 => twos
        case 3 => threes
        case 4 => fours
        case 5 => fives
        case 6 => sixes
        case 7 => sevens
        case 8 => eights
        case 9 => nines
      }
      r + bitVector(n/10)
    }
  }

  //5027^3 == 127035954683
  def main(args: Array[String]) {
    val map = new HashMap[Int, ListBuffer[Int]]()
    timed {
      for (i <- 200 to 10000) {
        val li = i.toLong
        val bv = bitVector(li*li*li)
        val list = map.getOrElseUpdate(bv, ListBuffer())
        list.append(i)
        if (list.length == 5) println(list)

      }
    }
  }
}
