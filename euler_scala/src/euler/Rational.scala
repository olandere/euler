package euler

class Rational(val n: BigInt, val d: BigInt) extends Ordered[Rational] {

  override def toString = "" + n + "/" + d

  def +(operand: Rational): Rational = {
    Rational(n * operand.d + operand.n * d, d * operand.d)
  }

  def -(operand: Rational): Rational = {
    Rational(n * operand.d - operand.n * d, d * operand.d)
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

  def compare(that: Rational) = {
    (n * that.d).compare(that.n * d)
  }

  def mediant(that: Rational) = {
    Rational(n + that.n, d + that.d)
  }

  def toFloat = {
    n.toFloat/d.toFloat
  }

  def toDouble = {
    n.toDouble/d.toDouble
  }
}

object Rational {
  implicit def intToRational(i: Int) = Rational(BigInt(i))

  def apply(n: BigInt, d: BigInt) = {
    require(d != 0)
    val t = n.gcd(d)
    new Rational(n / t, d / t)
  }

  def apply(n: BigInt) = new Rational(n, BigInt(1))
}