// P04 (*) Find the number of elements of a list.
//     Example:
//     scala> length(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 6

object P04{
  def length(list: List[Int]) = list.length
  def lengthRecursive(list: List[Int]): Int = list match{
    case Nil => 0
    case _ :: tail => 1 + lengthRecursive(tail)
  }
  def main(args: Array[String]): Unit = {
    println(length(List(1,1,2,3,5,8)))
    println(length(List(1)))
        
    println(lengthRecursive(List(1,1,2,3,5,8)))
    println(lengthRecursive(List(1)))
  }
}
