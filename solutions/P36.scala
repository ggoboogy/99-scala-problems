// P36 (**) Determine the prime factors of a given positive integer (2).
//     Construct a list containing the prime factors and their multiplicity.
//
//     scala> 315.primeFactorMultiplicity
//     res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
//
//     Alternately, use a Map for the result.
//     scala> 315.primeFactorMultiplicity
//     res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)

object P36{
  // Recursive version only
  // encodeRecursive() is modified from P10
	def encodeRecursive(list: List[Any]) = {
    def innerPack(packed: List[List[Any]],
                                  list: List[Any]): List[List[Any]] = list match{
      case Nil => packed
      case h::tail if (!packed.isEmpty && packed.last.head == h) =>
        innerPack(packed.init:::List(packed.last:::List(h)), tail)
      case h::tail => innerPack(packed:::List(List(h)), tail)
    }
    def innerEncode(encoded: List[(Any, Int)],
                            list: List[List[Any]]): List[(Any, Int)] = list match{
      case Nil => encoded
      case h::tail => innerEncode(encoded:::List((h.head, h.length)), tail)
    }
    innerEncode(List(), innerPack(List(), list))
  }
  // innerPrimeFactorRecursive() is same with P35
  def primeFactorMultiplicityRecursive(num: Int) = {
    def innerPrimeFactorRecursive(n: Int,
                                            current: Int): List[Int] = {
      if (current > num) Nil
      else if (n%current == 0) 
        current::innerPrimeFactorRecursive(n/current, 2)
      else innerPrimeFactorRecursive(n, current + 1)
    }
    encodeRecursive(innerPrimeFactorRecursive(num, 2))
  }
  def main(args: Array[String]): Unit = {
    println(primeFactorMultiplicityRecursive(315))// List((3,2), (5,1), (7,1))
    println(primeFactorMultiplicityRecursive(6)) // List((2,1), (3,1))
    println(primeFactorMultiplicityRecursive(12)) // List((2,2), (3,1))
    println(primeFactorMultiplicityRecursive(147)) // List((3,1), (7,2))
    println(primeFactorMultiplicityRecursive(17)) // List((17,1))
    println(primeFactorMultiplicityRecursive(48)) // List((2,4), (3,1))
    println(primeFactorMultiplicityRecursive(90)) // List((2,1), (3,2), (5,1))
  }
}
