package euler

import MathUtils._

object Euler72 {

  // 303963552391
  def main(args: Array[String]) {
    timed {
      var r = 0l
      for (i<- 2 to 1000000) {
        r = r + totient(i).toLong
      }
      println(r)
    }
  }
}
