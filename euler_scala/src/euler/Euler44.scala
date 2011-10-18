
object Euler44 {

  def quadratic(a : Double, b : Double, c : Double) = {
    val q = -0.5 * (b + math.signum(b)*math.sqrt(b*b-4*a*c))
    (q/a, c/q)
  }

  def isPentagonal(n: Int) = {
    val (r1, r2) = quadratic(3, -1, -2*n)
    (r1 > 0 && pentagonal(r1.toInt) == n) || (r2 > 0 && pentagonal(r2.toInt) == n)
  }

  def pentagonal(n : Int) = {
    n*(3*n-1)/2
  }

  def main(args: Array[String]) {
    val suspects = for (a <- 1 to 5000;
      b <- a + 1 to 5000 ;
      if isPentagonal(pentagonal(a) + pentagonal(b));
      if isPentagonal(pentagonal(b) - pentagonal(a))) yield ((a, b))
    println(suspects)
    println(pentagonal(2167) - pentagonal(1020))
  }
}