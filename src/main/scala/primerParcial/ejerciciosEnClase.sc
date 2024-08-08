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

