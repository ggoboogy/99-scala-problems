// P03 (*) Find the Kth element of a list.
//     By convention, the first element in the list is element 0.
//
//     Example:
//     scala> nth(2, List(1, 1, 2, 3, 5, 8))
//     res0: Int = 2

object P03{
    def nth(n: Int, list: List[Int]) = {
        if (n >= list.length) "[*] list size error"
        else list(n)
    }
    def nth_recursive(n: Int, list: List[Int]): Any = (n, list) match{
        case (0, h :: _) => h
        case (n, _ :: tail) => nth_recursive(n-1, tail)
        case (_, Nil) => "[*] list size error"
    }
    def main(args: Array[String]): Unit = {
        println(nth(3, List(1,1,2,3,5,8)))
        println(nth(1, List(1)))
        
        println(nth_recursive(3, List(1,1,2,3,5,8)))
        println(nth_recursive(1, List(1)))
    }
}
