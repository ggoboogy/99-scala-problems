// P13 (**) Run-length encoding of a list (direct solution).
//     Implement the so-called run-length encoding data compression method
//     directly.  I.e. don't use other methods you've written (like P09's
//     pack); do all the work directly.
//
//     Example:
//     scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

object P13{
  // Recursive version only
  def encodeDirectRecursive(list: List[Any]) = {
    def innerEncode(encoded: List[(Int, Any)], 
                                list: List[Any]): List[(Int, Any)] = list match{
      case Nil => encoded
      case h::tail if (!encoded.isEmpty && encoded.last._2 == h) =>
        innerEncode(encoded.init:::List((encoded.last._1 + 1, h)), tail)
      case h::tail => innerEncode(encoded:::List((1, h)), tail)
    }
    innerEncode(List(), list)
  }
  def main(args: Array[String]): Unit = {
    println(encodeDirectRecursive(List(3, 4, 5, 6, 6, 5, 5, 5, 4, 3, 3)))
    println(encodeDirectRecursive(List('a, 'a, 'a, 'a, 'b,
      'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encodeDirectRecursive(List()))
  }
}
