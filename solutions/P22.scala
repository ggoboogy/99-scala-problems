// P22 (*) Create a list containing all integers within a given range.
//     Example:
//     scala> range(4, 9)
//     res0: List[Int] = List(4, 5, 6, 7, 8, 9)

object P22{
  // Recursive version only
  def rangeRecursive(start: Int, end: Int): List[Any] = {
    if (start <= end) start::rangeRecursive(start + 1, end)
    else Nil
  }
  def main(args: Array[String]): Unit = {
    println(rangeRecursive(4, 9))
    println(rangeRecursive(10, 13))
    println(rangeRecursive(1, 10))
  }
}
