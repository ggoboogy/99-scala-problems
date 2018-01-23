// P19 (**) Rotate a list N places to the left.
//     Examples:
//     scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//
//     scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

object P19{
  def rotate(num: Int, list: List[Any]) = {
    if (num < 0) list.drop(list.length + num):::list.take(list.length + num)
    else list.drop(num):::list.take(num)
  }
  def rotateRecursive(num: Int, list: List[Any]) = {
    def innerRotate(front: List[Any], rear: List[Any],
                            list: List[Any], len: Int): List[Any] = list match{
      case Nil => front:::rear
      case h::tail if ((num > 0 && len - list.length < num) ||
        num < 0 && len - list.length < len + num) =>
        innerRotate(front, rear:::List(h), tail, len)
      case h::tail => innerRotate(front:::List(h), rear, tail, len)
    }
    innerRotate(List(), List(), list, list.length)
  }
  def main(args: Array[String]): Unit = {
    println(rotate(2, List(3, 4, 5, 6, 6, 5)))
    println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(rotate(-1, List('a, 'b)))
    
    println(rotateRecursive(2, List(3, 4, 5, 6, 6, 5)))
    println(rotateRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(rotateRecursive(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(rotateRecursive(-1, List('a, 'b)))
  }
}
