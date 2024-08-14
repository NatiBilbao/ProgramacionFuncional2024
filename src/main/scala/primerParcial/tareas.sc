//modelo de sustitución de gcd(51,95)
def gcd(a:Int, b:Int) : Int = if(b == 0) a else gcd(b, a%b)
gcd(51,95)

//Ejercicios Tarea 1
//Exercise 1: Pascal’s Triangle

def pascal(a: Int, b: Int): Int = if (a == 0 || a == b) 1 else pascal(a - 1, b - 1) + pascal(a, b - 1)

//Exercise 2: Parentheses Balancing

def balance(chars: List[Char]): Boolean = {

  def balanceHelper(chars: List[Char], openCount: Int): Boolean = if (chars.isEmpty) openCount == 0 else if (openCount < 0) false
    else {
      val newCount = chars.head match {
        case '(' => openCount + 1
        case ')' => openCount - 1
        case _ => openCount
      }
      balanceHelper(chars.tail, newCount)
    }
  balanceHelper(chars, 0)
}

//Exercise 3: Counting Change

def countChange(dinero: Int, monedas: List[Int]): Int = {
  def countWays(dinero: Int, monedas: List[Int]): Int = if (dinero == 0) 1
    else if (dinero < 0 || monedas.isEmpty) 0
    else countWays(dinero - monedas.head, monedas) + countWays(dinero, monedas.tail)
  countWays(dinero, monedas)
}
