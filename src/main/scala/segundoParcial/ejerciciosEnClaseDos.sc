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

//Solución

abstract class Expr

case class Number(n: Int) extends Expr

case class Sum(a: Expr, b: Expr) extends Expr

case class Mul(a: Expr, b: Expr) extends Expr

def eval(e: Expr) : Int = e match {
  case Number(n) => n
  case Sum(a,b) => eval(a) + eval(b)
  case Mul(a, b) => eval(a) * eval(b)

}

def show(e: Expr) : String = {
  def whithPar(x : Expr) : String = x match {
    case Sum(_, _) => "(" + show(x) + ")"
    case _ => show(x)
  }
  e match {
    case Number(n) => n.toString
    case Sum(a, b) => show(a) + "+" + show(b)
    case Mul(a, b) => whithPar(a) + "*" + whithPar(b)
  }
}

show(Mul(Sum(Number(3), Number(4)), Sum(Number(5), Number(6))))

//Realizar una clase abstracta figura geometrica y hacer sub clases cuadrado, rectangulo, circulo y
// afuera de estas clases hacer un método llamado areas que toma una figura geometrica y devuelve el area en double

//Solución
abstract class FigGeometrica

case class Cuadrado(l: Double) extends FigGeometrica
case class Rectangulo(a: Double, b: Double) extends FigGeometrica
case class Circulo(r: Double)

val c = Cuadrado(4.5)
c.l

def area(f: FigGeometrica) = f match {
  case Cuadrado(l) => l * l
  case Rectangulo(a, b) => (b * a)
  case Circulo(r) => r * r*Math.PI
}

//Realizar una función insertSort que toma una lista de Int para ordenarla

def insertSort(l : List[Int]) : List[Int] = {
  def insert(e: Int, l1: List[Int]) : List[Int] = l1 match {
    case Nil => List(e)
    case h :: t => if(e < h) e :: l1 else h :: insert(e, t)
  }
  l match {
    case Nil => Nil
    case h :: t => insert(h, insertSort(t))
  }
}

//Realizar una funcion para encontrar el elemento más grande de una lista de ints, el ultimo y el penultimo

//Solución

def max(l : List[Int]) : Int = l match {
  case Nil => throw new UnsupportedOperationException()
  case List(e) => e
  case h :: h1 :: t => if(h < h1) max(h1 :: t) else max(h :: t)
}

def last[T](l : List[T]) : T = l match {
  case Nil => throw new UnsupportedOperationException()
  case List(e) => e
  case _ :: t => last(t)
}

def penultimate[T](l : List[T]) : T = l match {
  case Nil | List(_)=> throw new UnsupportedOperationException()
  case List(a,_) => a
  case _ :: t => penultimate(t)
}

//Realizar la inversa

def reverse[T](l : List[T]) : List[T] = {
  def inner(l1 : List[T], l2 : List[T]) : List[T] = l1 match {
    case Nil => l2
    case h :: t => inner(t, h :: l2)
  }

  inner(l, Nil)
}

