package euler

class Rational(val n: BigInt, val d: BigInt) {
  
  override def toString = "" + n + "/" + d

  def +(operand: Rational): Rational = {
    Rational(n * operand.d + operand.n * d, d * operand.d)
  }

  def *(operand: Rational): Rational = {
    Rational(n * operand.n, d * operand.d)
  }

  def /(operand: Rational): Rational = {
    Rational(n*operand.d, d*operand.n)
  }

  def numeratorBigger: Boolean = {
    n.toString().length() > d.toString().length()
  }

//  def reduce: Rational = {
//    val t = MathUtils.gcd(r.n, r.d)
//    new Rational(r.n / t, r.d / t)
//  }
}

object Rational {
  implicit def intToRational(i: Int) = Rational(BigInt(i))

  def apply(n: BigInt, d: BigInt) = {
    val t = n.gcd(d)
    new Rational(n / t, d / t)
  }

  def apply(n: BigInt) = new Rational(n, BigInt(1))
}