// P23 (**) Extract a given number of randomly selected elements from a list.
//     Example:
//     scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
//     res0: List[Symbol] = List('e, 'd, 'a)
//
//     Hint: Use the answer to P20.

object P23{
  // Recursive version only
  // removeAtRecursive() is same with P20
  def removeAtRecursive(idx: Int, list: List[Any]) = {
    def innerRemoveAt(result: List[Any], list: List[Any],
                                            len: Int): (List[Any], Any) = {
      if ((len - list.length) == idx) (result:::list.tail, list.head)
      else innerRemoveAt(result:::List(list.head), list.tail, len)
    }
    innerRemoveAt(List(), list, list.length)
  }
  def randomSelectRecursive(num: Int, list: List[Any]): List[Any] = {
    if (num > 0) {
      val (remain, selected) =
        removeAtRecursive(scala.util.Random.nextInt(list.length), list)
      selected::randomSelectRecursive(num - 1, remain)
    }
    else Nil
  }
  def main(args: Array[String]): Unit = {
    println(randomSelectRecursive(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    println(randomSelectRecursive(4, List('a, 'b, 'c, 'd, 'e, 'f, 'g,
      'h, 'i, 'j, 'k)))
    println(randomSelectRecursive(1, List('a, 'b, 'c, 'd)))
  }
}
