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