package euler

import io.Source._

object Euler59 {

  def keyStream(a: Byte, b: Byte, c: Byte): Stream[Byte] = Stream.cons(a, Stream.cons(b, Stream.cons(c, keyStream(a, b, c))))

  def asString(l: List[Byte]): String = {
    l.map(b => b.toChar).mkString
  }

  def main(args: Array[String]) {
    val lines: List[Byte] = fromFile("/Users/eolander/euler/euler_scala/src/euler/cipher1.txt").mkString.split(",").map(_.trim.toByte).toList

    val z = for {a <- 97 to 122
                 b <- 97 to 122
                 c <- 91 to 122
                 val l = lines.zip(keyStream(a.toByte, b.toByte, c.toByte)).map(x => (x._1 ^ x._2).toByte)
                 val s = asString(l)
                 if s.contains(" the ") && s.contains(" and ")
    } yield (l)

    println(asString(z(0)))
    println(z(0).foldLeft(0)(_ + _)) //107359
  }
}