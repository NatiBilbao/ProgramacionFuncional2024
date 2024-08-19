def sumInts(a: Int, b: Int): Int = {
  def inner(i : Int, acc : Int) : Int = if (i > b) acc else inner(i + 1, acc + i)

  inner(a, 0)
}

def cube(x : Int): Int = x*x*x
def sumCubes(a: Int, b: Int): Int = {
  def inner(i : Int, acc : Int) : Int = if (i > b) acc else inner(i + 1, acc + cube(i))
  inner(a, 0)
}

def factorial(n: Int) = {
  def inner(i:Int, acc : Int) : Int = if(i == 0) acc else inner(i-1, acc*i)
  inner(n, 1)
}

//Función que factoriza las tres funciones anteriores ya que la función de inner siempre es la misma
def sumF(a: Int, b: Int, f: Int => Int) = {
  def inner(i: Int, acc: Int) : Int = if(i > b) acc else inner(i+1, acc + f(i))
  inner(a, 0)
}

def megaFunc(a: Int, b: Int, f: Int => Int, op: (Int, Int)=>Int, acc0:Int) ={
  def inner(i: Int, acc:Int) : Int = if(i > b) acc else inner(i+1, op(f(i), acc))

  inner(a, acc0)
}

//Función anonima
sumF(4,5, x => 2*x)

//Placeholder notation

def oneLastFactorial(n: Int) = megaFunc(1, n, _, _*_, acc0 = 1)

//Funciones que fabrican funciones

def fabricadorDeSaludos(l: String) = {
  (name: String) => if (l == "ES") "Hola" + name
  else if (l == "EN") "Hello" + name
  else if (l == "FR") "Bonjour" + name
  else "JAU"
}

def saludoEnEspañol = fabricadorDeSaludos("ES")
saludoEnEspañol(" Alejandro")
saludoEnEspañol(" Diego")

def saludoEnFrances = fabricadorDeSaludos("FR")
saludoEnFrances(" René")

fabricadorDeSaludos("EN")(" Manuel")

def fabricadorDeMegaFunc(op: Int => Int, acc0: Int) = (a: Int, b: Int, f: Int => Int) =>{
  def inner(i: Int, acc:Int) : Int = {
    if (i > b) acc else inner(i+1, op(f(i), acc))
    inner(a, acc0)
  }
}

//Currificación
//def f(a: Int, b: Int) = a+b
//def fc(a: Int) = (b: Int) => a+b

//Forma resumida de escribir currificado las funciones anteriores

def f(a: Int, b: Int) = a+b
def fc(a: Int)(b: Int) = a+b

def forall(p : Int => Boolean)(f: Int => Int) =



