abstract class Expr

case class Number(n: Int) extends Expr

case class Sum(a: Expr, b: Expr) extends Expr

case class Sub(a: Expr, b: Expr) extends Expr

case class Mul(a: Expr, b: Expr) extends Expr

case class Div(a: Expr, b: Expr) extends Expr

def eval(e: Expr) : Int = e match {
  case Number(n) => n
  case Sum(a,b) => eval(a) + eval(b)
}

eval(Sum(Number(3), Number(4)))