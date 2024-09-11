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

//Realizar la función pack

//Solución no tail recursive

def packTrucho[T](l : List[T]) : List[List[T]] = l match {
  case Nil => Nil
  case h :: t => {
    val(a,b) = l span(_ == h)
    a :: packTrucho(b)
  }
}

//Solución tail recursive

def pack[T](l : List[T]) : List[List[T]] = {
  def inner(l1 : List[T], acc : List[List[T]]) : List[List[T]] = l1 match {
    case Nil => acc.reverse
    case h :: _ => {
      val (a, b) = l span(_ == h)
      inner(b, a :: acc)
    }
  }
  inner(l, Nil)
}

//Realizar una función encode

def encodeLargo[T](l : List[T]) : List[(T, Int)] = {
  def inner(l1 : List[T], acc : List[(T, Int)]) : List[(T, Int)] = l1 match {
    case Nil => acc.reverse
    case h :: _ => {
      val (a, b) = l span(_ == h)
      inner(b, (h, a.size) :: acc)
    }
  }
  inner(l, Nil)
}

//Solución más corta

def encode[T](l : List[T]) = pack(l) map(l => (l.head, l.size))

//Realizar una función que encuentre el número máximo en una lista no usar inner solo usar reducción

def maxLargo(l : List[Int]) : Int = l reduceLeft((a,b) => if (a>b) a else b)

//Solución mas corta

def max(l : List[Int]) : Int = l reduceLeft(_ max _)

