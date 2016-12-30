package recfun

object Main {
  def main(args: Array[String]) {
    /*
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    */

    /*
    println("Balance")
    println("(if (zero? x) max (/ 1 x))\n" + balance("(if (zero? x) max (/ 1 x))".toList))
    println("I told him (that it’s not (yet) done). (But he wasn’t listening)\n" + balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(":-)\n" + balance(":-)".toList))
    println("())(\n" + balance("())(".toList))
    */

    //println("countChange")

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      // this is a special case
      if(c == 0 || c == r) 1
      // this is the recursive step
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def trim(count: Int, list: List[Char]): Int = {
        if(list.isEmpty) count
        else{
          if(count < 0){
            -1
          }
          else if(list.head == '('){
            //println(count)
            trim(count+1, list.tail)
          }
          else if(list.head == ')'){

            trim(count-1, list.tail)
          }
          else{
            //println(count)
            trim(count, list.tail)
          }

        }
      }

      val balance = trim(0, chars)
      //println("Balance is " + balance)
      if(balance == 0){
        return true
      }
      false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money < 0) 0
      else if(money == 0) 1
      else if(coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
