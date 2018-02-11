// P50 (***) Huffman code.
//     First of all, consult a good book on discrete mathematics or algorithms
//     for a detailed description of Huffman codes!
//    
//     We suppose a set of symbols with their frequencies, given as a list of
//     (S, F) Tuples.  E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16),
//     ("e", 9), ("f", 5)).  Our objective is to construct a list of (S, C)
//     Tuples, where C is the Huffman code word for the symbol S.
//
//     scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
//     res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))

object P50{
  // Recursive version only
  def huffman(list: List[(String, Int)]) = {
    def generateTree(list: List[List[(String, Int)]]):
                                            List[List[(String, Int)]]= {
      if (list.length == 1) list
      else {
        val sorted = list.sortBy(_.last._2)
        val parent_node = (sorted.head.last._1+sorted.tail.head.last._1,
          sorted.head.last._2 + sorted.tail.head.last._2)
        val merged = (parent_node::sorted.head:::sorted.tail.head).sortBy(_._2)
        generateTree(List(merged):::sorted.tail.tail)
      }
    }
    def generateHuffmanCode(list: List[(String, Int)],
             result: List[(String, String)]): List[(String, String)] = {
      if (list.length == 0) result
      else if (result.length == 0)
        generateHuffmanCode(list.tail, result:::List((list.head._1, "")))
      else {
        var code: String = ""
        for( r <- result if r._1.contains(list.head._1)) code = r._2
        code += result.length % 2
        generateHuffmanCode(list.tail, result:::List((list.head._1, code)))
      }
    }
    var code = generateHuffmanCode(
      generateTree(list.map(List(_))).flatten.sortWith(_._2 > _._2), List())
    var result: List[Any] = List()
    for ( element <- list )
      result = result:::List((element._1, code.find(_._1 == element._1).get._2))
    result
  }
  def main(args: Array[String]): Unit = {
    println(huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))))
    // => List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
    
    println(huffman(List(("a", 24), ("b", 12), ("c", 10), ("d", 8), ("e", 8))))
    // => List((a,0), (b,111), (c,110), (d,101), (e,100))
  }
}
