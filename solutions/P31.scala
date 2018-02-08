// P31 (**) Determine whether a given integer number is prime.
//     scala> 7.isPrime
//     res0: Boolean = true

// A fairly naive implementation for primality testing is simply: a number is
// prime if it it not divisible by any prime number less than or equal to its
// square root.
// Here, we use a Stream to create a lazy infinite list of prime numbers.  The
// mutual recursion between `primes` and `isPrime` works because of the limit
// on `isPrime` to the square root of the number being tested.

object P31{
  // Recursive version only
  def isPrimeRecursive(num: Int): Boolean = num match{
    case 1 => false
    case 2 => true
    case _ => !((2 until num/2+1) exists (num%_ == 0))
  }
  def main(args: Array[String]): Unit = {
    // prime number test
    println(isPrimeRecursive(2))
    println(isPrimeRecursive(3))
    println(isPrimeRecursive(17))
    println(isPrimeRecursive(31))
    println(isPrimeRecursive(977))

    // non-prime number test
    println(isPrimeRecursive(1))
    println(isPrimeRecursive(4))
    println(isPrimeRecursive(10))
    println(isPrimeRecursive(33))
    println(isPrimeRecursive(99))
    println(isPrimeRecursive(121))
  }
}
