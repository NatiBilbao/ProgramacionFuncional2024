//Realizar un función mySpan que toma como párametros l y f y devuelve una tupla

def mySpan[T](l: List[T], f: T => Boolean): (List[T], List[T]) = {
  def inner(l1: List[T], acc: List[T]): (List[T], List[T]) = l1 match {
    case Nil => (acc.reverse, Nil)
    case h :: t if f(h) => inner(t, h :: acc)
    case _ => (acc.reverse, l1)
  }
  inner(l, Nil)
}
//Tarea del link realizar los Problema 8, 12, 14, 16

