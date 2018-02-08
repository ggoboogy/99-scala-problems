// P16 (**) Drop every Nth element from a list.
//     Example:
//     scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

object P16{
  // Recursive version only
  def dropRecursive(num: Int, list: List[Any]) = {
    def innerDrop(result: List[Any], list: List[Any],
                                          len: Int): List[Any] = list match{
      case Nil => result
      case h::tail if ((len - list.length + 1) % num == 0) =>
        innerDrop(result, tail, len)
      case h::tail => innerDrop(result:::List(h), tail, len)
    }
    innerDrop(List(), list, list.length)
  }
  def main(args: Array[String]): Unit = {
    println(dropRecursive(2, List(3, 4, 5, 6, 6, 5)))
    println(dropRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(dropRecursive(1, List('a, 'b)))
  }
}
