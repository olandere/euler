package euler

import MathUtils._

/*
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?
 */
//(134043,134044,134045,134046)
object Euler47 {
  def main(args: Array[String]) {
   timed{
    val list = for (n <- intStream(210); if (primeFactors(n).distinct.length == 4)) yield n
   // val list2 = for (((x ,y), z) <- (list.zip(list.tail)).zip(list.tail.tail); if x+1 == y && y+1 == z) yield (x, y, z)
    val list3 = for ((((w ,x), y), z) <- ((list.zip(list.tail)).zip(list.tail.tail)).zip(list.tail.tail.tail); if w+1 == x && x+1 == y && y+1 == z) yield (w, x, y, z)
//    (list.head + 1) == list.tail.head
   // println(list.take(400).toList)
    println(list3.head)
  //  println(list2.head)
          }
  }
}
