//Realizar una funcion myFilter qur tome como parametros l y f

def myFilter[T](l : List[T], f : T => Boolean) = {
  def inner(l1: List[T], acc : List[T]) : List[T] = l1 match {
    case Nil => acc.reverse
    case h :: t if f(h) => inner(t, h :: acc)
    case _ :: t => inner(t, acc)
  }
  inner(l, Nil)
}

//Realizar myFilterNot que no acepta false

def myFilterNot[T](l : List[T], f : T => Boolean) = myFilter(l, !f(_))

//Realizar un función mySpan que toma como párametros l y f y devuelve una tupla

def mySpan[T](l : List[T], f : T => Boolean) : (List[T], List[T]) = ???