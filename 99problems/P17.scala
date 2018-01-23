// P17 (*) Split a list into two parts.
//     The length of the first part is given.  Use a Tuple for your result.
//
//     Example:
//     scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

object P17{
  // Recursive version only
  def splitRecursive(num: Int, list: List[Any]) = {
    def innerSplit(result: List[List[Any]], list: List[Any],
                                      len: Int): List[List[Any]] = list match{
      case Nil => result
      case h::tail if (result.isEmpty || (len - list.length) == num) =>
        innerSplit(result:::List(List(h)), tail, len)
      case h::tail =>
        innerSplit(result.init:::List(result.last:::List(h)), tail, len)
    }
    innerSplit(List(), list, list.length)
  }
  def main(args: Array[String]): Unit = {
    println(splitRecursive(2, List(3, 4, 5, 6, 6, 5)))
    println(splitRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(splitRecursive(1, List('a, 'b)))
  }
}
