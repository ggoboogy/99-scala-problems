// P39 (*) A list of prime numbers.
//     Given a range of integers by its lower and upper limit, construct a list
//     of all prime numbers in that range.
//
//     scala> listPrimesinRange(7 to 31)
//     res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)

object P39{
  // Recursive version only
  // isPrimeRecursive() is same with P31
  def isPrimeRecursive(num: Int): Boolean = num match{
    case 1 => false
    case 2 => true
    case _ => !((2 until num/2+1) exists (num%_ == 0))
  }
  def listPrimesinRangeRecursive(range: Range) = {
    def innerListPrimesinRange(current: Int): List[Int] = {
      if (current > range(range.length-1)) Nil
      else if (isPrimeRecursive(current))
        current::innerListPrimesinRange(current + 1)
      else innerListPrimesinRange(current + 1)
    }
    innerListPrimesinRange(range(0))
  }
  def main(args: Array[String]): Unit = {
    println(listPrimesinRangeRecursive(7 to 31))
    println(listPrimesinRangeRecursive(2 to 5))
    println(listPrimesinRangeRecursive(97 to 105))
    println(listPrimesinRangeRecursive(1 to 100))
  }
}
