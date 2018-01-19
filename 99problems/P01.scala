// P01 (*) Find the last element of a list.
//     Example:
//     scala> last(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 8

object P01{
    def last(list: List[Int]) = {
        if (list.isEmpty) "[*] empty list error"
        else list.last
    }
    def last_recursive(list: List[Int]): Any = list match{
        case h :: Nil => h
        case _ :: tail => last_recursive(tail)
        case _ => "[*] empty list error"
    }
    def main(args: Array[String]): Unit = {
        println(last(List(1,1,2,3,5,8)))
        println(last(List()))
        
        println(last_recursive(List(1,1,2,3,5,8)))
        println(last_recursive(List()))
    }
}
