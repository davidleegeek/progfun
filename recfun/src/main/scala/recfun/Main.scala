package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    println(countChange(4,List(1,2)))
    println(countChange(300,List(5,10,20,50,100,200,500)))
    println(countChange(301,List(5,10,20,50,100,200,500)))
    println(countChange(300,List(500,5,50,100,20,200,10)))
    
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    println(!balance(":-)".toList))
    println(!balance("())(".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if (c < 0 || r < 0 || c > r) 0
    else if (c == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      def balanceIter(chars: List[Char], left: Int): Boolean = {
        if (chars.isEmpty) left == 0
        else if (left < 0) false
        else if (chars.head == '(') balanceIter(chars.tail, left + 1)
        else if (chars.head == ')') balanceIter(chars.tail, left - 1)
        else balanceIter(chars.tail, left)
      }
      balanceIter(chars, 0)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }
  
}
