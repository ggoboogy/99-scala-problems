// P10 (*) Run-length encoding of a list.
//     Use the result of problem P10 to implement the so-called run-length
//     encoding data compression method.  Consecutive duplicates of elements are
//     encoded as tuples (N, E) where N is the number of duplicates of the
//     element E.
//
//     Example:
//     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

object P10{
  // Recursive version only
  // innerPack() is the same code with innerPack() of P09
  def encodeRecursive(list: List[Any]) = {
    def innerPack(packed: List[List[Any]],
                                  list: List[Any]): List[List[Any]] = list match{
      case Nil => packed
      case h::tail if (!packed.isEmpty && packed.last.head == h) =>
        innerPack(packed.init:::List(packed.last:::List(h)), tail)
      case h::tail => innerPack(packed:::List(List(h)), tail)
    }
    def innerEncode(encoded: List[(Int, Any)],
                            list: List[List[Any]]): List[(Int, Any)] = list match{
      case Nil => encoded
      case h::tail => innerEncode(encoded:::List((h.length, h.head)), tail)
    }
    innerEncode(List(), innerPack(List(), list))
  }
  def main(args: Array[String]): Unit = {
    println(encodeRecursive(List(3, 4, 5, 6, 6, 5, 5, 5, 4, 3, 3)))
    println(encodeRecursive(List('a, 'a, 'a, 'a, 'b, 
      'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
