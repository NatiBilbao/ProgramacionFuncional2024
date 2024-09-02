abstract class List[+T]{
  def isEmpty : Boolean
  def head : T
  def tail : List[T]
  def prepend[S >: T](el : S) : List[S] = new Cons(el, tail=this)
}

class Cons[+T](val head: T, val tail : List[T]) extends List[T]{
  def isEmpty = false
}

object Nil extends List[Nothing]{
  def isEmpty = true
  def head = throw new NoSuchElementException()
  def tail = throw new NoSuchElementException()
}