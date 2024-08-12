//Realizar una funcion para:
//def and(b1: Boolean, b2:Boolean) = ???
//def or(b1: Boolean, b2:Boolean) = ???
//def not(b1: Boolean) = ???

//def and(b1: Boolean, b2:  => Boolean) = if (b1) b2 else false

//def or(b1: Boolean, b2: => Boolean) = if (b1) true else b2

//Solución
def and(b1: Boolean, b2:  => Boolean) = if(b1) b2 else false

def or(b1: Boolean, b2: => Boolean) = if (b1) true else b2

def not(b1: Boolean) = if(b1) false else true

//Realizar la implementación en código de GCD

def gcd(a:Int, b:Int) : Int = if(b == 0) a else gcd(b, a%b)

gcd(51,95)

//Realizar la factorial de:
//def fact(n:Int):Int=???

def facto(n: Int): Int = if (n <= 1) 1 else n * facto(n - 1)

facto(4)

//Solución 2

def fact(n:Int): Int = if(n==0) 1 else n*fact(n-1)
fact(4)

//Realizar fibonacci simple
//def fibo(n:Int): Int = ???

def fibo(n:Int): Int = if(n<=1) 1 else fibo(n-1) + fibo(n-2)
fibo(9)

//Realizar la suma de enteros
// def sumInts(a: Int, b: Int) = ???

def sumInts(a: Int, b: Int): Int = {
  def inner(i : Int, acc : Int) : Int = if (i > b) acc else inner(i + 1, acc + i)

  inner(a, 0)
}

sumInts(3,7)

//Solución sin recursividad de cola

def sumInt(a: Int, b: Int) : Int = if (a == b) a else a + sumInt(a + 1, b)

sumInt(3,7)

//Realizar la suma de los cubos
//def sumCubes(a: Int, b: Int) = ???

def cubos()


