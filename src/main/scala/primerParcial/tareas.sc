//modelo de sustitución de gcd(51,95)
def gcd(a:Int, b:Int) : Int = if(b == 0) a else gcd(b, a%b)
gcd(51,95)