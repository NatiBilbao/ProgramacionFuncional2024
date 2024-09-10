//Map
List(1,2,3,4) map(_ + "Mississippi")

def map[T,U](l : List[T], f : T => U) : List[U] = {
  def inner(l1 : List[T], acc : List[U]) : List[U] = l1 match {
    case Nil => acc
    case h :: t => inner(t, f(h) :: acc)
  }
  inner(l, Nil).reverse
}

List("Hola", "Yaaaa", "", "Este es un string más largo") map (_.length)

def myFilter2[T](l : List[T], f : T => Boolean) = {
  def inner(l1: List[T], acc : List[T]) : List[T] = l1 match {
    case Nil => acc.reverse
    case h :: t => if (f(h)) inner(t, h :: acc) else inner(t, acc)
  }
  inner(l, Nil)
}

def unique[T](l : List[T]) = {
  def inner(l1 : List[T], acc: List[T]) : List[T] = l1 match {
    case Nil => acc.reverse
    case h :: t if acc contains h => inner(t, acc)
    case h :: t => inner(t, h :: acc)
  }
  inner(l, Nil)
}

def unique2[T](l : List[T]) = {
  def inner(l1: List[T], acc : List[T]) : List[T] = l1 match {
    case Nil => acc.reverse
    case h :: t => inner(l1 filterNot(_ == h), h :: acc)
  }
  inner(l, Nil)
}