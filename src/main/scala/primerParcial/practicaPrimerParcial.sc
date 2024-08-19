//Representation
type Set = Int => Boolean
def contains(s: Set, elem: Int): Boolean = s(elem)

//2.1 Basic Functions on Sets

def singletonSet(elem: Int): Set = (x: Int) => x == elem

def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

//2.2 Queries and Transformations on Sets
def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > 1000) true
    else if (contains(s, a) && !p(a)) false
    else iter(a + 1)
  }
  iter(-1000)
}

def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, x => f(x) == y)

//Solución de la práctica

