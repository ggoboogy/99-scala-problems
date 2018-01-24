// P25 (*) Generate a random permutation of the elements of a list.
//     Hint: Use the solution of problem P23.
//
//     Example:
//     scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//     res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

object P25{
  // removeAtRecursive() is same with P20
  def removeAtRecursive(idx: Int, list: List[Any]) = {
    def innerRemoveAt(result: List[Any], list: List[Any],
                                            len: Int): (List[Any], Any) = {
      if ((len - list.length) == idx) (result:::list.tail, list.head)
      else innerRemoveAt(result:::List(list.head), list.tail, len)
    }
    innerRemoveAt(List(), list, list.length)
  }
  // Built-in version
  def randomPermute(list: List[Any]) = scala.util.Random.shuffle(list)

  // Recursive version
  def randomPermuteRecursive(list: List[Any]) = {
    def innerRandomPermute(list: List[Any], result: List[Any]): List[Any] = {
      if (list.length == 0) result
      else {
        val (remain, n) =
          removeAtRecursive(scala.util.Random.nextInt(list.length), list)
        innerRandomPermute(remain, result:::List(n))
      }
    }
    innerRandomPermute(list, List())
  }
  def main(args: Array[String]): Unit = {
    println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
    println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f, 'g,
      'h, 'i, 'j, 'k)))
    println(randomPermute(List('a, 'b, 'c, 'd)))
    
    println(randomPermuteRecursive(List('a, 'b, 'c, 'd, 'e, 'f)))
    println(randomPermuteRecursive(List('a, 'b, 'c, 'd, 'e, 'f, 'g,
      'h, 'i, 'j, 'k)))
    println(randomPermuteRecursive(List('a, 'b, 'c, 'd)))
  }
}
