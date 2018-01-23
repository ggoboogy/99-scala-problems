// P20 (*) Remove the Kth element from a list.
//     Return the list and the removed element in a Tuple.  Elements are
//     numbered from 0.
//
//     Example:
//     scala> removeAt(1, List('a, 'b, 'c, 'd))
//     res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

object P20{
  // Recursive version only
  def removeAtRecursive(idx: Int, list: List[Any]) = {
    def innerRemoveAt(result: List[Any], list: List[Any],
                                            len: Int): (List[Any], Any) = {
      if ((len - list.length) == idx) (result:::list.tail, list.head)
      else innerRemoveAt(result:::List(list.head), list.tail, len)
    }
    innerRemoveAt(List(), list, list.length)
  }
  def main(args: Array[String]): Unit = {
    println(removeAtRecursive(2, List(3, 4, 5, 6, 6, 5)))
    println(removeAtRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(removeAtRecursive(1, List('a, 'b, 'c, 'd)))
  }
}
