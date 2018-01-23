// P15 (**) Duplicate the elements of a list a given number of times.
//     Example:
//     scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//     res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

object P15{
  // Recursive version only
  def duplicateNRecursive(num: Int, list: List[Any]) = {
    def innerDuplicateN(result: List[Any], list: List[Any]): List[Any] = list match{
      case Nil => result
      case h::tail => innerDuplicateN(result:::List.fill(num)(h), tail)
    }
    innerDuplicateN(List(), list)
  }
  def main(args: Array[String]): Unit = {
    println(duplicateNRecursive(4, List(3, 4, 5, 6, 6, 5)))
    println(duplicateNRecursive(3, List('a, 'b, 'c, 'c, 'd)))
    println(duplicateNRecursive(1, List()))
  }
}
