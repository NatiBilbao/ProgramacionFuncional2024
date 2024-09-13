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

//Problema 8

def compress[T](l: List[T]): List[T] = {
  def inner(current: List[T], acc: List[T]): List[T] = current match {
    case Nil => acc.reverse
    case h :: Nil => (h :: acc).reverse
    case h :: t if acc.isEmpty || acc.head != h => inner(t, h :: acc)
    case _ :: t => inner(t, acc)
  }

  inner(l, Nil)
}

//Problema 12

def decode[T](l: List[(Int, T)]): List[T] = {
  l.flatMap { case (count, elem) => List.fill(count)(elem) }
}

//Problema 14

def duplicate[T](l: List[T]): List[T] = {
  l.flatMap(elem => List(elem, elem))
}

//Problema 16

def drop[T](n: Int, l: List[T]): List[T] = {
  def inner(count: Int, current: List[T]): List[T] = current match {
    case Nil => Nil
    case h :: t => if (count % n == 0) inner(count + 1, t)
    else h :: inner(count + 1, t)
  }

  inner(1, l)
}
