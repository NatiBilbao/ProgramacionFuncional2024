abstract class Set {
  def contains(x: Int) : Boolean
  def add(x: Int) : Set
  def union(o : Set) : Set
}

//representación de set como árbol binario

abstract class NonEmptySet(e : Int, left : Set, right : Set) extends Set {
  def contains(x: Int): Boolean = if(x == e) true else if (x < e) left contains x else right contains x
  def add(x : Int) : Set = if(e == x) this else if(x < e) new NonEmptySet(e, left add x, right) else new NonEmptySet(e, left, right add x)
  def union(o : Int) : Set = left union (right union o) add e
}

class EmptySet extends Set {
  def contains(x: Int): Boolean = false
  def add(x: Int): Set = new NonEmptySet(x, new EmptySet, new EmptySet)
  def union(o : Set) : Set = o
}



