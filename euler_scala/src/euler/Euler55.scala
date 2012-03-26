package euler

import annotation.tailrec

object Euler55 {

  def revNum(n: BigInt) = BigInt(n.toString().reverse)

  def isPalindrome(n: BigInt) = revNum(n) == n

  def revSum(n: BigInt) = n + revNum(n)

  def isLychrel(n: Int) = {

    @tailrec
    def isLychrel(n: BigInt, cnt: Int): Boolean = {
      if (cnt > 50) {
        true
      }
      else {
        val t = revSum(n)
        if (isPalindrome(t)) {
          false
        } else {
          isLychrel(t, cnt + 1)
        }
      }
    }
    isLychrel(BigInt(n), 0)
  }

  def main(args: Array[String]) {
    val lnums = for (i <- 1 to 10000; if isLychrel(i)) yield {
      i
    }
    println(lnums.length)
  }

}
