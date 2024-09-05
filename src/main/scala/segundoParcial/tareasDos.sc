class Fraction(x: Int, y: Int) {

  require(y > 0, "El denominador debe ser positivo")
  def num = x
  def denom = y

  def add(o: Fraction): Fraction =
    new Fraction(denom * o.num + num * o.denom, denom * o.denom)

  def nullFraction: Fraction = new Fraction(0, 1)

  def neg: Fraction = new Fraction(-num, denom)

  def sub(o: Fraction): Fraction =
    new Fraction(num * o.denom - o.num * denom, denom * o.denom)

  def div(o: Fraction): Fraction =
    new Fraction(num * o.denom, denom * o.num)

  def inver: Fraction = new Fraction(denom, num)

  def lt(o: Fraction): Boolean = num * o.denom < o.num * denom

  def le(o: Fraction): Boolean = num * o.denom <= o.num * denom

  def gt(o: Fraction): Boolean = num * o.denom > o.num * denom

  def ge(o: Fraction): Boolean = num * o.denom >= o.num * denom
}

//Corección tarea
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
}

//Realizar size recursivo:
// def size [T](l : List[T]) = ???

def size[T](l: List[T]): Int = l match {
  case Nil => 0
  case _ :: t => 1 + size(t)
}
