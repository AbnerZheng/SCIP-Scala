import scala.annotation.tailrec

class Rational(n: Int, d: Int) {
  require(d != 0)
  private val g = gcd(n.abs, d.abs)
  val numer = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def +(that: Rational): Rational = {
    new Rational(numer * that.denom + denom * that.numer,
      denom * that.denom)
  }

  def +(that: Int): Rational = {
    this + new Rational(that)
  }

  def -(that: Rational): Rational = {
    this + -that
  }

  def -(that: Int): Rational = {
    this + new Rational(-that)
  }

  def unary_- = {
    new Rational(-numer, denom)
  }

  def *(that: Rational): Rational = {
    new Rational(numer * that.numer,
      denom * that.denom)
  }

  def *(that: Int): Rational = {
    new Rational(numer * that, denom)
  }

  def /(that: Rational): Rational = {
    new Rational(numer * that.denom, denom * that.numer)
  }

  def /(that: Int): Rational = {
    new Rational(numer, denom * that)
  }

  @tailrec
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  override def toString = if (denom == 1) "" + numer else numer + "/" + denom
}

implicit def intToRational(x:Int) = new Rational(x)

val a = new Rational(4)
val b = new Rational(3, 2)
val i = 1
val sum = a + b
val prod = a * b
val sub = a - i
val sum2 = b + i

val prod2 = i * a

val filesHere = (new java.io.File(".")).listFiles
for (file <- filesHere)
  println(file)
