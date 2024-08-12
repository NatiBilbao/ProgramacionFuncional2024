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

