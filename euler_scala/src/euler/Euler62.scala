package euler

import MathUtils._
import collection.mutable.{ListBuffer, HashMap}

object Euler62 {

  def bitVector(n: Long): Int = {
    if (n == 0) {
      0
    }
    else {
      val r = 1 << 3 * (n % 10)
      r + bitVector(n / 10)
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
