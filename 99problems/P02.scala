// P02 (*) Find the last but one element of a list.
//     Example:
//     scala> penultimate(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 5

object P02{
  def penultimate(list: List[Int]) = {
    if (list.length < 2) "[*] empty list error"
    else list.init.last
  }
  def penultimateRecursive(list: List[Int]): Any = list match{
    case h :: _ :: Nil => h
    case _ :: tail => penultimateRecursive(tail)
    case _ => "[*] empty list error"
  }
  def main(args: Array[String]): Unit = {
    println(penultimate(List(1,1,2,3,5,8)))
    println(penultimate(List()))
        
    println(penultimateRecursive(List(1,1,2,3,5,8)))
    println(penultimateRecursive(List()))
  }
}
