//Completar def add(x : Int) : Set = ???
abstract class Set {
  def contains(x: Int) : Boolean
  def add(x: Int) : Set
}

//representación de set como árbol binario

class NonEmptySet(e : Int, left : Set, right : Set) extends Set {
  def contains(x: Int): Boolean = if(x == e) true else if (x < e) left contains x else right contains x
  def add(x : Int) : Set = if(x < e) new NonEmptySet(e, left add x, right) else if(x > e) new NonEmptySet(e, left, right add x) else this
}

//Pattern Maching
//Completar las siguiente funciones:
// 1) Completar eval
// 2) def show(e:Expr) : String

abstract class Expr

case class Number(n: Int) extends Expr

case class Sum(a: Expr, b: Expr) extends Expr

case class Sub(a: Expr, b: Expr) extends Expr

case class Mul(a: Expr, b: Expr) extends Expr

case class Div(a: Expr, b: Expr) extends Expr

def eval(e: Expr) : Int = e match {
  case Number(n) => n
  case Sum(a,b) => eval(a) + eval(b)
  case Mul(a, b) => eval(a) * eval(b)
  case Div(a, b) => eval(a) / eval(b)
}

eval(Sum(Number(3), Number(4)))