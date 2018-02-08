// P41 (**) A list of Goldbach compositions.
//     Given a range of integers by its lower and upper limit, print a list of
//     all even numbers and their Goldbach composition.
//
//     scala> printGoldbachList(9 to 20)
//     10 = 3 + 7
//     12 = 5 + 7
//     14 = 3 + 11
//     16 = 3 + 13
//     18 = 5 + 13
//     20 = 3 + 17
//
//     In most cases, if an even number is written as the sum of two prime
//     numbers, one of them is very small.  Very rarely, the primes are both
//     bigger than, say, 50.  Try to find out how many such cases there are in
//     the range 2..3000.
//
//     Example (minimum value of 50 for the primes):
//     scala> printGoldbachListLimited(1 to 2000, 50)
//     992 = 73 + 919
//     1382 = 61 + 1321
//     1856 = 67 + 1789
//     1928 = 61 + 1867

object P41{
  // Recursive version only
  // isPrimeRecursive() is same with P31
  // printGoldbachList() is modified from P40
  def isPrimeRecursive(num: Int): Boolean = num match{
    case 1 => false
    case 2 => true
    case _ => !((2 until num/2+1) exists (num%_ == 0))
  }
  def printGoldbachList(range: Range): Unit = {
    def innerGoldbach(num: Int, current: Int): (Int, Int) = {
      if (isPrimeRecursive(current) && isPrimeRecursive(num - current))
        (current, num - current)
      else innerGoldbach(num, current + 1)
    }
    for(i <- range if (i%2 == 0 && i > 2)) {
      val result = innerGoldbach(i, 2)
      println(s"$i = ${result._1} + ${result._2}")
    }
  }
  def printGoldbachListLimited(range: Range, limit: Int): Unit = {
    def innerGoldbach(num: Int, current: Int): (Int, Int) = {
      if (isPrimeRecursive(current) && isPrimeRecursive(num - current))
        (current, num - current)
      else innerGoldbach(num, current + 1)
    }
    for(i <- range if (i%2 == 0 && i > 2)) {
      val result = innerGoldbach(i, 2)
      if(result._1 >= limit && result._2 >= limit)
        println(s"$i = ${result._1} + ${result._2}")
    }
  }
  def main(args: Array[String]): Unit = {
    printGoldbachList(9 to 20)
    printGoldbachList(14 to 50)
    printGoldbachList(1 to 10)

    printGoldbachListLimited(1 to 2000, 50)
    printGoldbachListLimited(5 to 50, 7)
    printGoldbachListLimited(20 to 100, 11)
  }
}
