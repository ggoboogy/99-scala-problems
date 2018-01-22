// P06 (*) Find out whether a list is a palindrome.
//     Example:
//     scala> isPalindrome(List(1, 2, 3, 2, 1))
//     res0: Boolean = true

object P06{
  def isPalindrome(list: List[Int]) = list.reverse == list
  def isPalindromeRecursive(list: List[Int]): Boolean = list match{
    case Nil => true
    case h :: Nil => true // if there is one element
    case list => (list.head == list.last && isPalindromeRecursive(list.tail.init))
  }
  def main(args: Array[String]): Unit = {
    println(isPalindrome(List(1,2,3,2,1)))
    println(isPalindrome(List(3,4,5,6,6,5,4,3)))
    println(isPalindrome(List(1,2,3)))
    println(isPalindrome(List(50,6,4,6,50)))
        
    println(isPalindromeRecursive(List(1,2,3,2,1)))
    println(isPalindromeRecursive(List(3,4,5,6,6,5,4,3)))
    println(isPalindromeRecursive(List(1,2,3)))
    println(isPalindromeRecursive(List(50,6,4,6,50)))
  }
}
