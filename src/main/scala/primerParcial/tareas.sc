//modelo de sustitución de gcd(51,95)
def gcd(a:Int, b:Int) : Int = if(b == 0) a else gcd(b, a%b)
gcd(51,95)

//Ejercicios Tarea 1
//Exercise 1: Pascal’s Triangle

def pascal(a: Int, b: Int): Int = if (a == 0 || a == b) 1 else pascal(a - 1, b - 1) + pascal(a, b - 1)

//Exercise 2: Parentheses Balancing

def balance(chars: List[Char]): Boolean = {
  def balanceHelper(chars: List[Char], openCount: Int): Boolean = {
    if (chars.isEmpty) openCount == 0
    else if (openCount < 0) false
    else {
      val newCount = chars.head match {
        case '(' => openCount + 1
        case ')' => openCount - 1
        case _ => openCount
      }
      balanceHelper(chars.tail, newCount)
    }
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

//Solución tarea

//Ejercicio 1

def pascales(c: Int, r: Int) : Int = if(c == 0 || c == r) 1 else pascales(c, r-1) + pascales(c-1, r-1)

//Ejercicio 2

def balances(chars: List[Char]) : Boolean = {
  def inner(chars: List[Char], acc : Int) : Boolean =
    if(chars.isEmpty) acc == 0
    else if (acc < 0) false
    else if (chars.head == '(' ) inner(chars.tail, acc + 1)
    else if (chars.head == ')' ) inner(chars.tail, acc - 1)
    else inner(chars.tail, acc)

  inner(chars, acc = 0)
}

//Ejercicio 3

def countChanges(money: Int, coins: List[Int]) : Int = {
  if (money == 0) 1
  else if (coins.isEmpty || money < 0) 0
  else countChanges(money - coins.head, coins) + countChanges(money, coins.tail)
}


