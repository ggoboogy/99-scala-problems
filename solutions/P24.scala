// P24 (*) Lotto: Draw N different random numbers from the set 1..M.
//     Example:
//     scala> lotto(6, 49)
//     res0: List[Int] = List(23, 1, 17, 33, 21, 37)

object P24{
  // Recursive version only
  def lottoRecursive(n: Int, m: Int) = {
    def innerLotto(cnt: Int, result: List[Any]): List[Any] = {
      if (cnt == n) result
      else {
        val num = scala.util.Random.nextInt(m) + 1
        if (result.contains(num)) innerLotto(cnt, result)
        else innerLotto(cnt + 1, num::result)
      }
    }
    innerLotto(0, List())
  }
  def main(args: Array[String]): Unit = {
    println(lottoRecursive(6, 49))
    println(lottoRecursive(3, 20))
    println(lottoRecursive(20, 100))
    println(lottoRecursive(0, 100))
  }
}
