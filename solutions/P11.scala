// P11 (*) Modified run-length encoding.
//     Modify the result of problem P11 in such a way that if an element has no
//     duplicates it is simply copied into the result list.  Only elements with
//     duplicates are transferred as (N, E) terms.
//
//     Example:
//     scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

object P11{
  // Recursive version only
  // innerPack() is the same code with innerPack() of P09
  def encodeModifiedRecursive(list: List[Any]) = {
    def innerPack(packed: List[List[Any]],
                              list: List[Any]): List[List[Any]] = list match{
      case Nil => packed
      case h::tail if (!packed.isEmpty && packed.last.head == h) =>
        innerPack(packed.init:::List(packed.last:::List(h)), tail)
      case h::tail => innerPack(packed:::List(List(h)), tail)
    }
    def innerEncodeModified(encoded: List[Any],
                              list: List[List[Any]]): List[Any] = list match{
      case Nil => encoded
      case h::tail if (h.length == 1) =>
        innerEncodeModified(encoded:::List(h.head), tail)
      case h::tail => innerEncodeModified(encoded:::List((h.length, h.head)), tail)
    }
    innerEncodeModified(List(), innerPack(List(), list))
  }
  def main(args: Array[String]): Unit = {
    println(encodeModifiedRecursive(List(3, 4, 5, 6, 6, 5, 5, 5, 4, 3, 3)))
    println(encodeModifiedRecursive(List('a, 'a, 'a, 'a, 'b, 
      'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
