// P05 (*) Reverse a list.
//     Example:
//     scala> reverse(List(1, 1, 2, 3, 5, 8))
//     res0: List[Int] = List(8, 5, 3, 2, 1, 1)

object P05{
  def reverse(list: List[Int]) = list.reverse
  def reverseRecursive(list: List[Int]): List[Int] = list match{
    case Nil => Nil
    case h :: tail => reverseRecursive(tail) ::: List(h)
  }
  def main(args: Array[String]): Unit = {
    println(reverse(List(1,1,2,3,5,8)))
    println(reverse(List(1,2,3)))
        
    println(reverseRecursive(List(1,1,2,3,5,8)))
    println(reverseRecursive(List(1,2,3)))
  }
}
