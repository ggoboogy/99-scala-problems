// P21 (*) Insert an element at a given position into a list.
//     Example:
//     scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
//     res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

object P21{
  // Recursive version only
  def insertAtRecursive(input: Any, idx: Int, list: List[Any]) = {
    def innerInsertAt(result: List[Any], list: List[Any],
                                                    len: Int): List[Any] = {
      if ((len - list.length) == idx) result:::List(input):::list
      else innerInsertAt(result:::List(list.head), list.tail, len)
    }
    innerInsertAt(List(), list, list.length)
  }
  def main(args: Array[String]): Unit = {
    println(insertAtRecursive('new, 1, List('a, 'b, 'c, 'd)))
    println(insertAtRecursive('test, 2, List(3, 4, 5, 6, 6, 5)))
    println(insertAtRecursive(5, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}
