// P40 (**) Goldbach's conjecture.
//     Goldbach's conjecture says that every positive even number greater than 2
//     is the sum of two prime numbers.  E.g. 28 = 5 + 23.  It is one of the
//     most famous facts in number theory that has not been proved to be correct
//     in the general case.  It has been numerically confirmed up to very large
//     numbers (much larger than Scala's Int can represent).  Write a function
//     to find the two prime numbers that sum up to a given even integer.
//
//     scala> 28.goldbach
//     res0: (Int, Int) = (5,23)

object P40{
  // Recursive version only
  // isPrimeRecursive() is same with P31
  def isPrimeRecursive(num: Int): Boolean = num match{
    case 1 => false
    case 2 => true
    case _ => !((2 until num/2+1) exists (num%_ == 0))
  }
  def goldbachRecursive(num: Int) = {
    def innerGoldbach(current: Int): (Int, Int) = {
      if (isPrimeRecursive(current) && isPrimeRecursive(num - current))
        (current, num - current)
      else innerGoldbach(current + 1)
    }
    if (num <= 2 || num%2 == 1) (None, None)
    else innerGoldbach(2)
  }
  def main(args: Array[String]): Unit = {
    println(goldbachRecursive(28)) // should be (5, 23)
    println(goldbachRecursive(120)) // should be (7, 113)
    println(goldbachRecursive(82)) // should be (3, 79)
    println(goldbachRecursive(2)) // should be (None, None)
    println(goldbachRecursive(11)) // should be (None, None)
    println(goldbachRecursive(1)) // should be (None, None)
  }
}
