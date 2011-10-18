object Euler53 {

  def prodTo(b: BigInt, e: BigInt): BigInt = {
    val one: BigInt = 1
    (b + 1 to e).foldLeft(one)(_ * _)
  }

  def facSimp(n: BigInt, r: BigInt, d: BigInt): BigInt = {
    if (r > d) prodTo(r, n) / prodTo(0, d) else prodTo(d, n) / prodTo(0, r)
  }

  def combinations(n: BigInt, r: BigInt): BigInt = {
    facSimp(n, r, n - r)
  }

  def main(args: Array[String]) {
    val result = for (n <- 1 to 100;
                      r <- 1 to n;
                      val c = combinations(n, r);
                      if (c > 1000000)
    ) yield (c)
    println(result.size)
  }
}