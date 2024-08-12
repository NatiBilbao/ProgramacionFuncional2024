"Hello world"
val x = 2

x+1

def f(x: Int) = x

f(3)

2.+(2)

val  g: Int => Int = f

g(4)

def suma(x: Int, y: Int)=x+y

def abs(x: Int) = if(x>=0) x else -x
abs(-1)

def gcd(a:Int, b:Int) : Int = if(b == 0) a else gcd(b, a%b)

gcd(51,95)

//Recursividad de cola

def factTR(n : Int, acc: Int):Int = if (n==0) acc else factTR(n-1, n*acc)

//Fibonacci código con recursividad

//def fiboTR(n : Int, i: Int)
//Cuadrado de un número código con recursividad
def abs(a:Double) : Double = if(a<0) -a else a

def suficiente(x:Double, a:Double) = abs(x-a*a) < 0.0001 * x

def mejorar(x:Double, a:Double) = (a+x/a)/2

def sqrt(x:Double, a:Double) : Double = if(suficiente(x,0)) a else sqrt(x, mejorar(x,a))

