// P34 (**) Calculate Euler's totient function phi(m).
//     Euler's so-called totient function phi(m) is defined as the number of
//     positive integers r (1 <= r < m) that are coprime to m.  As a special
//     case, phi(1) is defined to be 1.
//
//     scala> 10.totient
//     res0: Int = 4

object P34{
  // Recursive version only
  // gcdRecursive() is same with P32
  def gcdRecursive(num1: Int, num2: Int) = {
    def innerGcdRecursive(k: Int): Int = {
      if (num1%k == 0 && num2%k == 0) k
      else innerGcdRecursive(k - 1)
    }
    innerGcdRecursive(math.min(num1, num2))
  }
  def totientRecursive(num: Int) = {
    def innerTotientRecursive(num: Int, current: Int): Int = current match{
      case 0 => 0
      case _ if (gcdRecursive(current, num) == 1) =>
        1 + innerTotientRecursive(num, current - 1)
      case _ => innerTotientRecursive(num, current - 1)
    }
    innerTotientRecursive(num, num)
  }
  def main(args: Array[String]): Unit = {
    println(totientRecursive(10))
    println(totientRecursive(6))
    println(totientRecursive(7))
    println(totientRecursive(11))
  }
}
