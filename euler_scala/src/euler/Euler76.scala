package euler

object Euler76 {

  var cache:Map[(BigInt, BigInt), BigInt] = Map.empty[(BigInt, BigInt), BigInt]
  var missCount = 0
  var hitCount = 0

  def p(n: BigInt): BigInt = {
    def p(k: BigInt, n: BigInt): BigInt = {
    //  println("(" + k + ", " + n + ")")
      cache.get((k, n)) match {
        case None => val r:BigInt = if (k > n) 0 else if (k == n) 1 else p(k + 1,n) + p(k,n - k)
          missCount+=1
          cache = cache + ((k, n) -> r)
          r
        case Some(v) =>
          hitCount+=1
          v
      }

   //   if (k > n) 0 else if (k == n) 1 else p(k + 1,n) + p(k,n - k)
    }
  //  val g = Memoize.memoize(p _)
  //  println(g)
    p(1,n)
  }


  def main(args: Array[String]) {
 //   val z = Memoize.memoize(p _)
 //   println(p(40)+ " hits: "+hitCount+", miss: "+missCount)
    hitCount = 0
    missCount = 0
   // println("Euler 76: "+(p(100)-1)+ " hits: "+hitCount+", miss: "+missCount)

    val z = for (i <- 500 to 600;
      if p(i) % 1000000 == 0) yield(i)
    println(z)
  }
}