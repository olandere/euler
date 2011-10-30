package euler

object Euler346 {

  val Repunit = """1+""".r

  def repunits(start: BigInt): Stream[BigInt] = Stream.cons(start, repunits(start*10+1))

  def intStream(start: BigInt): Stream[BigInt] = Stream.cons(start, intStream(start+1))

  val repUnitStream = repunits(111)

  var possibleSet: Set[BigInt] = Set()
  var strongRepunits: Set[BigInt] = Set(1)

  def updateSets(x : BigInt) {
//    if (possibleSet(x)) strongRepunits = strongRepunits + x
//    else possibleSet = possibleSet + x
    strongRepunits = strongRepunits + x
  }

  def main(args: Array[String]) {
    // any number n is itself in base n+1 or greater - n would not become a repunit
    // any number n is always a repunit (specifically 11) in base n-1
    // if n == 2^x - 1, it is a repunit, so log(n+1)/log(2) is an integer value
    // probably can then search from base 3 to base n-2
    
    println(MathUtils.toBase(32, 8))
    println(MathUtils.binaryLog(32))

    println(MathUtils.toBase(40, 39))
    println(MathUtils.fromBase(MathUtils.toBase(40, 39), 39))

    println(MathUtils.fromBase(100, 5))

    println(repunits(11).take(5).toList)

    println(MathUtils.quadratic(1, 1, -999))
    println(MathUtils.quadratic(1, 1, -1000000000000L+1))

    //println(MathUtils.binaryLog(1000000000000L)) == 40
    println(MathUtils.binaryLog(1000L))
    val term: Long = 1000000000000L
   // val term = 1000
   // val maxBase = 31
    val maxBase = 1000000
//    for {x <- intStream(2);
//      if (x < term);
    intStream(2).takeWhile(_<=maxBase).foreach{x =>
      val l = repUnitStream.map(MathUtils.fromBase(_, x)).takeWhile(_<term)
      println(x+": "+l.toList)
      l.foreach(updateSets(_))//.filter(_<50).toList)
    }
    println(strongRepunits.sum)
  }
}