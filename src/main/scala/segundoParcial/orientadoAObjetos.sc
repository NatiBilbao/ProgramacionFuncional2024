//Constructor
class Fraction(x: Int, y: Int) {

  require(y > 0, "El denominador debe ser positivo")
  def num = x
  def denom = y

  def add(o: Fraction): Fraction =
    new Fraction(denom * o.num + num * o.denom, denom * o.denom)

  def mul(o: Fraction) = new Fraction((num*o.num, denom*o.denom)

  def neg = new Fraction(-num, denom)

  def sub(o : Fraction) = add(o.neg)

  def div(o: Fraction) = mul(o.inver)

  def inver = new Fraction(denom, num)

  def lt(o: Fraction): Boolean = num * o.denom < denom * o.num

  def le(o: Fraction): Boolean = num * o.denom <= denom * o.num

  def gt(o: Fraction) = !le(0)

  def ge(o: Fraction) = !lt(0)

  override def toString = num + "/" + denom
}

val a = new Fraction(2,3)
val b = new Fraction(4,5)

a.add(b)

