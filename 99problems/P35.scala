// P35 (**) Determine the prime factors of a given positive integer.
//     Construct a flat list containing the prime factors in ascending order.
//
//     scala> 315.primeFactors
//     res0: List[Int] = List(3, 3, 5, 7)

object P35{
  // Recursive version only
  def primeFactorsRecursive(num: Int) = {
    def innerPrimeFactorsRecursive(n: Int, current: Int): List[Int] = {
      if (current > num) Nil
      else if (n%current == 0) 
        current::innerPrimeFactorsRecursive(n/current, 2)
      else innerPrimeFactorsRecursive(n, current + 1)
    }
    innerPrimeFactorsRecursive(num, 2)
  }
  def main(args: Array[String]): Unit = {
    println(primeFactorsRecursive(315)) // should be List(3, 3, 5, 7)
    println(primeFactorsRecursive(6)) // should be List(2, 3)
    println(primeFactorsRecursive(12)) // should be List(2, 2, 3)
    println(primeFactorsRecursive(147)) // should be List(3, 7, 7)
    println(primeFactorsRecursive(17)) // should be List(17)
    println(primeFactorsRecursive(48)) // should be List(2, 2, 2, 2, 3)
    println(primeFactorsRecursive(90)) // should be List(2, 3, 3, 5)
  }
}
