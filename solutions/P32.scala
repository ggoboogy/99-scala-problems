// P32 (**) Determine the greatest common divisor of two positive integer
//          numbers.
//     Use Euclid's algorithm.
//
//     scala> gcd(36, 63)
//     res0: Int = 9

object P32{
  def gcd(num1: Int, num2: Int): Int = {
    def innerGcd() = {
      for(k <- (1 to math.min(num1, num2)).reverse
           if (num1%k == 0 && num2%k == 0)) yield k
    }
    val result = innerGcd
    result(0)
  }
  def gcdRecursive(num1: Int, num2: Int) = {
    def innerGcdRecursive(k: Int): Int = {
      if (num1%k == 0 && num2%k == 0) k
      else innerGcdRecursive(k - 1)
    }
    innerGcdRecursive(math.min(num1, num2))
  }
  def main(args: Array[String]): Unit = {
    println(gcd(36, 63))
    println(gcd(24, 48))
    println(gcd(1, 51))
    println(gcd(2, 120))
    println(gcd(60, 48))
    
    println(gcdRecursive(36, 63))
    println(gcdRecursive(24, 48))
    println(gcdRecursive(1, 51))
    println(gcdRecursive(2, 120))
    println(gcdRecursive(60, 48))
  }
}
