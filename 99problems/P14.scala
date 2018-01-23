// P14 (*) Duplicate the elements of a list.
//     Example:
//     scala> duplicate(List('a, 'b, 'c, 'c, 'd))
//     res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

object P14{
  // Recursive version only
  def duplicateRecursive(list: List[Any]) = {
    def innerDuplicate(result: List[Any],
                                    list: List[Any]): List[Any] = list match{
      case Nil => result
      case h::tail => innerDuplicate(result:::List.fill(2)(h), tail)
    }
    innerDuplicate(List(), list)
  }
  def main(args: Array[String]): Unit = {
    println(duplicateRecursive(List(3, 4, 5, 6, 6, 5)))
    println(duplicateRecursive(List('a, 'b, 'c, 'c, 'd)))
    println(duplicateRecursive(List()))
  }
}
