abstract class Expr

//case class Sub(a: Expr, b: Expr) extends Expr

//case class Div(a: Expr, b: Expr) extends Expr

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

eval(Sum(Number(3), Number(4)))
show(Mul(Sum(Number(3), Number(4)), Sum(Number(5), Number(6))))

val e: Any = ???
e match {
  case List(x) => x
  case s : String => s.length
  case l : List[_] => l.size
  case _ => -1
}

val l : List[Int] = List(1,2,3)
val l1 = List(List(1,2), List(3,4), "YAAAAAA")

//Listas vacias

val l2 = List()
val l3 = Nil

//Lista con cons ::

val l4 = 1 :: Nil
val l5 = Nil.::(3).::(2).::(1)

l.head
l.tail
l.size
l.isEmpty

l match {
  case List(x) => x
  case List(a, b) => a + b
  case Nil => 1
  case l : List[Boolean] => l.head
  case h :: _ => h
  case x :: y :: List(a, b) :: z => x
}






