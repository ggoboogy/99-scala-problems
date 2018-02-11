// P46 (**) Truth tables for logical expressions.
//     Define functions and, or, nand, nor, xor, impl, and equ (for logical
//     equivalence) which return true or false according to the result of their
//     respective operations; e.g. and(A, B) is true if and only if both A and B
//     are true.
//
//     scala> and(true, true)
//     res0: Boolean = true
//
//     scala> xor(true. true)
//     res1: Boolean = false
//
//     A logical expression in two variables can then be written as a function of
//     two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
//    
//     Now, write a function called table2 which prints the truth table of a
//     given logical expression in two variables.
//
//     scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
//     A     B     result
//     true  true  true
//     true  false true
//     false true  false
//     false false false

// The trick here is not using builtins.  We'll define `not`, `and`, and `or`
// directly (using pattern matching), and the other functions in terms of those
// three.

object P46{
  // Built-in version only
  def and(a: Boolean, b: Boolean) = (a, b) match {
    case (true, true) => true
    case _ => false
  }
  def or(a: Boolean, b: Boolean) = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _ => false
  }
  def not(result: Boolean) = result match {
    case true => false
    case false => true
  }
  def nand(a: Boolean, b: Boolean) = not(and(a, b))
  def nor(a: Boolean, b: Boolean) = not(or(a, b))
  def xor(a: Boolean, b: Boolean) = not(equ(a, b))
  def impl(a: Boolean, b: Boolean) = or(not(a), b)
  def equ(a: Boolean, b: Boolean) = or(and(a, b), not(or(a, b)))
  def table2(function: (Boolean, Boolean) => Boolean) {
    println("A\tB\tresult")
    for{ a <- List(true, false); b <- List(true, false)}
    println(a + "\t" + b + "\t" + function(a, b))
  }
  def main(args: Array[String]): Unit = {
    println(and(true, true)) // true
    println(or(false, true)) // ture
    println(not(true)) // false
    println(nand(false, false)) // true
    println(nor(true, false)) // false
    println(xor(true, true)) // false
    println(impl(false, true)) // true
    println(equ(true, true)) // true
    println(equ(false, false)) // true
    println(equ(true, false)) // false
    println(equ(false, true)) // false

    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    table2((a: Boolean, b: Boolean) => xor(or(a, b), and(a, b)))
  }
}
