// P26 (**) Generate the combinations of K distinct objects chosen from the N
//          elements of a list.
//     In how many ways can a committee of 3 be chosen from a group of 12
//     people?  We all know that there are C(12,3) = 220 possibilities (C(N,K)
//     denotes the well-known binomial coefficient).  For pure mathematicians,
//     this result may be great.  But we want to really generate all the possibilities.
//
//     Example:
//     scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
//     res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

object P26{
  // Recursive version only
  def combinationsRecursive(k: Int, list: List[Any]): List[List[Any]] = {
    if (k == 1) list.map(List(_))
    else list match {
      case Nil => Nil
      case h::tail => (combinationsRecursive(k - 1, tail).map(h::_)
        :::combinationsRecursive(k, tail))
    }
  }
  def main(args: Array[String]): Unit = {
    println(combinationsRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f)))
    println(combinationsRecursive(3, List('a, 'b, 'c, 'd)))
    println(combinationsRecursive(2, List('a, 'b, 'c, 'd)))
  }
}
