// P07 (**) Flatten a nested list structure.
//     Example:
//     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//     res0: List[Any] = List(1, 1, 2, 3, 5, 8)

object P07{
  // Recursive version only
  def flattenRecursive(list: List[Any]): List[Any] = list match{
    case Nil => Nil
    case (h:List[Any])::tail => flattenRecursive(h):::flattenRecursive(tail)
    case (h:Any)::tail => h::flattenRecursive(tail)
  }
  def main(args: Array[String]): Unit = {
    println(flattenRecursive(List(List(1,1),2,List(3,List(5,8)))))
    println(flattenRecursive(List(3,4,5,6,6,5,4,3)))
    println(flattenRecursive(List(1,3,List(7,5),7,List(List(8,9,10),2))))
  }
}
