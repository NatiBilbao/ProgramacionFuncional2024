import scala.collection.immutable.Set

//Representation
type Sets = Int => Boolean
def containss(s: Set, elem: Int): Boolean = s(elem)

//2.1 Basic Functions on Sets

def singletonSets(elem: Int): Set = (x: Int) => x == elem

def unions(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

def intersects(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

def diffs(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

def filters(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

//2.2 Queries and Transformations on Sets
def foralls(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > 1000) true
    else if (contains(s, a) && !p(a)) false
    else iter(a + 1)
  }
  iter(-1000)
}

def existss(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

def maps(s: Set, f: Int => Int): Set = (y: Int) => exists(s, x => f(x) == y)

//Solución de la práctica

type Set = Int => Boolean

def contains(s: Set, elem: Int) : Boolean = s(elem)

//2.1

//1.

def singletonSet(elem: Int): Set = _ == elem

//2.

def union(s: Set, t: Set): Set = x => s(x) || t(x)
def intersect(s: Set, t: Set): Set = x => s(x) && t(x)
def diff(s: Set, t: Set): Set = x => s(x) && !t(x)

//3.
def filter(s: Set, p: Int => Boolean): Set = intersect(s,p)

def complement(s: Set) : Set = !s(_)
//2.2

//1.
def forall(s: Set, p: Int => Boolean): Boolean = {
  def inner(i: Int) : Boolean = ( i == 1001) || complement(diff(s,p))(i) && inner(i+1)
    inner(-1000)
}

//2.

def exists(s: Set, p: Int => Boolean): Boolean = !forall(s,complement(p))

//3.

def map(s: Set, f: Int => Int): Set = x => exists(s, f(_) == x)


