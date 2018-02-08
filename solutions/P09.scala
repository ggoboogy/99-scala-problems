// P09 (**) Pack consecutive duplicates of list elements into sublists.
//     If a list contains repeated elements they should be placed in separate
//     sublists.
//
//     Example:
//     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     packed0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
//																		List('a, 'a), List('d), List('e, 'e, 'e, 'e))

object P09{
  // Recursive version only
  def packRecursive(list: List[Any]) = {
    def innerPack(packed: List[List[Any]],
                              list: List[Any]): List[List[Any]] = list match{
      case Nil => packed
      case h::tail if (!packed.isEmpty && packed.last.head == h) =>
        innerPack(packed.init:::List(packed.last:::List(h)), tail)
      case h::tail => innerPack(packed:::List(List(h)), tail)
    }
    innerPack(List(),list)
  }
  def main(args: Array[String]): Unit = {
    println(packRecursive(List(3, 4, 5, 6, 6, 5, 5, 5, 4, 3, 3)))
    println(packRecursive(List('a, 'a, 'a, 'a, 'b, 
      'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
