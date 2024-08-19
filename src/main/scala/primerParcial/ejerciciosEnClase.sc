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

//FOF
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

def cube(x : Int): Int = x*x*x
def sumCubes(a: Int, b: Int): Int = {
  def inner(i : Int, acc : Int) : Int = if (i > b) acc else inner(i + 1, acc + cube(i))
  inner(a, 0)
}

sumCubes(2,4)

//Solucion sin def cubes

def sumCube(a: Int, b: Int): Int = {
  def inner(i : Int, acc : Int) : Int = if (i > b) acc else inner(i + 1, acc + i*i*i)
  inner(a, 0)
}

//Realizar la factorial de un número
//def factorial(n:Int) = ???

//Solución sin recursividad de cola
def factorial(n: Int): Int = if (n <= 1) 1 else n * factorial(n - 1)

//Solución con recursividad de cola

def factori(n: Int) = {
  def inner(i:Int, acc : Int) : Int = if(i == 0) acc else inner(i-1, acc*i)
  inner(n, 1)
}

//Realizar la suma de los factoriales entre a y b
//def sumFacts(a: Int, b:Int) = ???

def factTR(n:Int):Int = {
  def inner(i:Int, acc:Int):Int = if (i==0) acc else inner(i-1, i*acc)
  inner(n, 1)
}

def sumFacts(a: Int, b: Int): Int = {
  def inner(i : Int, acc : Int) : Int = if (i > b) acc else inner(i + 1, acc + factTR(i))
  inner(a, 0)
}

def sumF(a: Int, b: Int, f: Int => Int) = {
  def inner(i: Int, acc: Int) : Int = if(i > b) acc else inner(i+1, acc + f(i))
  inner(a, 0)
}

//Realizar la suma de enteros entre a y b usando sumF
// def sumEven(a: Int, b: Int) = ???

//Solucion
def sumEven(a: Int, b: Int) = sumF(a,b, x => if(x%2 == 0) x else 0)

//Realizar la funcion para todos los producctos entre a y b
//def proF(a: Int, b: Int, f: Int => Int) = ???

def prodF(a:Int, b:Int, f:Int=> Int) = {
  def inner(i : Int, acc : Int) : Int = if(i>b) acc else inner(i+1, acc*f(i))
  inner(a, 1)
}

//Realizar la factorial de un número usando prodF
//def anotherFactorial(a: Int) = ???

//Solución

def anotherFactorial(n: Int) = prodF(1, n, x=>x)

//Realizar una función que factorice las funciones sumF y prodF
//def megaFunc() ???

//Solución
def megaFunc(a: Int, b: Int, f: Int => Int, op: (Int, Int)=>Int, acc0:Int) ={
  def inner(i: Int, acc:Int) : Int = if(i > b) acc else inner(i+1, op(f(i), acc))

  inner(a, acc0)
}

//Realizar una función
//def yetAnotherFactorial (a: Int) = ???

//Solución
def yetAnotherFactorial (n: Int) = megaFunc(1, n, x=>x, (x,y)=>x*y, acc0 = 1) //Idenity = x=>x se puede usar las dos son igual

//Realizar la composición de funciones

def f(x : Int) = 2*x
def g(x : Int) = x*x

def comp(f: Int => Int, g : Int => Int)(x : Int) : Int = f(g(x))
comp(f,g)(3)

//Solución 2

def compo(g: Int => Int, f: Int => Int) : Int => Int = (n: Int) => g(f(n))

//Solución más pequeña

def compos(g: Int => Int, f: Int => Int)(n: Int) = g(f(n))
def fs(n: Int) = n-1
def gs(n: Int) = n*n

def h : Int => Int = compos(g,f)

//Realizar la suma, resta y multiplicación de funciones
// def join(op)(f,g)

def join(op : (Int, Int) => Int)(f : Int => Int, g : Int => Int)(x : Int) = op(f(x), g(x))
join(_+_)(f,g)(3)

//Realizar la función para saber si es positiva usar de ejemplo -> (-1000,1000)
// def esPositivo(f: Int => Int) : Boolean

def esPositivo(f: Int => Int) : Boolean ={
  def inner(i: Int) : Boolean = if(i == 1001) true else if(f(i) < 0) false else inner(i + 1)
  inner(-1000)
}

//Solución más compacta

def sonPositivos(f: Int => Int) : Boolean ={
  def inner(i: Int) : Boolean = i == 1001 || (if(f(i) < 0) false else inner(i + 1))
  inner(-1000)
}
 //Soluición mas compacta 2

def sonPosi(f: Int => Int) : Boolean ={
  def inner(i: Int) : Boolean = i == 1001 || ((f(i) >= 0) && inner(i + 1))
  inner(-1000)
}

//Realizar una función para todos los resultados de f para todos los valores entre min y max cumple el predicado p
def forall(p: Int => Boolean, min: Int, max: Int)(f: Int => Int): Boolean = (min to max).forall(value => p(f(value)))

//Solución

def foralls(p: Int => Boolean)(f: Int => Int) = {
  def inner(i: Int) : Boolean = i == 1001 || (p(f(i))) && inner(i + 1)
    inner(-1000)
}

//Realizar la función existe entre un predicado y una función

def exist(p: Int => Boolean)(f: Int => Int) = {
  def inner(i: Int) : Boolean = i == 1001 || (p(i)) && inner(i + 1)
  inner(-1000)
}
