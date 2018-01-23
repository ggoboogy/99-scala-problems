// P12 (**) Decode a run-length decoded list.
//     Given a run-length code list generated as specified in problem P10,
//     construct its uncompressed version.
//
//     Example:
//     scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
//     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

object P12{
  // Recursive version only
  def decodeRecursive(list: List[(Int, Any)]) = {
    def innerDecode(decoded: List[Any],
                            list: List[(Int, Any)]): List[Any] = list match{
      case Nil => decoded
      case h::tail => innerDecode(decoded:::List.fill(h._1)(h._2), tail)
    }
    innerDecode(List(), list)
  }
  def main(args: Array[String]): Unit = {
    println(decodeRecursive(List((1,3), (1,4), (1,5), (2,6), (3,5), (1,4), (2,3))))
    println(decodeRecursive(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))))
    println(decodeRecursive(List()))
  }
}
