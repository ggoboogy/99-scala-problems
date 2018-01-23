// P08 (**) Eliminate consecutive duplicates of list elements.
//     If a list contains repeated elements they should be replaced with a
//     single copy of the element.  The order of the elements should not be
//     changed.
//
//     Example:
//     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

object P08{
  // Recursive version only
  def compressRecursive(list: List[Any]): List[Any] = list match{
    case Nil => Nil
    case h::Nil => List(h)
    case h::tail if (h == tail.head) => compressRecursive(tail)
    case h::tail => h::compressRecursive(tail)
  }
  def main(args: Array[String]): Unit = {
    println(compressRecursive(List(3,4,5,6,6,5,5,5,4,3,3)))
    println(compressRecursive(List('a, 'a, 'a, 'a, 'b, 
      'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
