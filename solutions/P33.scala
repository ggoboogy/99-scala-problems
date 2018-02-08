//P33 (*) Determine whether two positive integer numbers are coprime.
//     Two numbers are coprime if their greatest common divisor equals 1.
//
//     scala> 35.isCoprimeTo(64)
//     res0: Boolean = true

object P33{
  // built-in verison only
  // gcdRecursive() is same with P32
  def isCoprimeTo(num1: Int, num2: Int) = {
    if (gcdRecursive(num1, num2) == 1) true
    else false
  }
  def gcdRecursive(num1: Int, num2: Int) = {
    def innerGcdRecursive(k: Int): Int = {
      if (num1%k == 0 && num2%k == 0) k
      else innerGcdRecursive(k - 1)
    }
    innerGcdRecursive(math.min(num1, num2))
  }
  def main(args: Array[String]): Unit = {
    // Coprime test
    println(isCoprimeTo(1, 51))
    println(isCoprimeTo(2, 71))
    println(isCoprimeTo(3, 40))
    println(isCoprimeTo(11, 20))

    // non-coprime test
    println(isCoprimeTo(36, 63))
    println(isCoprimeTo(24, 48))
    println(isCoprimeTo(2, 120))
    println(isCoprimeTo(60, 48))
  }
}
