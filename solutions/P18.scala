// P18 (**) Extract a slice from a list.
// //     Given two indices, I and K, the slice is the list containing the
// elements
// //     from and including the Ith element up to but not including the Kth
// //     element of the original list.  Start counting the elements with 0.
// //
// //     Example:
// //     scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// //     res0: List[Symbol] = List('d, 'e, 'f, 'g)

object P18{
  // Recursive version only
  def sliceRecursive(i: Int, k: Int, list: List[Any]) = {
    def innerSlice(result: List[Any], list: List[Any],
                                          len: Int): List[Any] = list match{
      case Nil => result
      case h::tail if (len - list.length >= i && len - list.length < k) =>
        innerSlice(result:::List(h), tail, len)
      case h::tail => innerSlice(result, tail, len)
    }
    innerSlice(List(), list, list.length)
  }
  def main(args: Array[String]): Unit = {
    println(sliceRecursive(2, 4, List(3, 4, 5, 6, 6, 5)))
    println(sliceRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(sliceRecursive(1, 8, List('a, 'b)))
  }
}
