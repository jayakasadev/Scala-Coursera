/*
Set vs Sequence
  sets are unordered
    elements of set do not have a predetermined order in which they appear in the
    set
    sets do not have duplicates
    fundamental operation on sets is contains


most operations on sequences are available on sets
 */
val fruit = Set("apple", "apple")
val nums = (1 to 7).toSet

nums map (_ + 2)
nums.nonEmpty
fruit filter (_.startsWith("app"))

/*
N Queens

eight queens problem is to place 8 queens on a chessboard so that no queen is threatened by another
  can't have 2 queens on the same row, col, or diagonal

1 solution is to place a queen on each row
once we have placed k-1 queens
  one must place kth queen in a column where it not "in check" with any other queen

leads to a recursive solution
suppose we have already generated all the solutions consisting of placing k-1 queens on board of size n

each solution is represented by a list of length k-1 containting the number of columns  between 0 and n-1

column number of the queen in the k-1th row comes first in the list
  followed by the column number of the queen in the k-2th queen

solution set is therefore represented as a set of lists with one element for each solution

lastly
  place kth queen
    generate all possible extensions of each solution preceded by a new queen
 */

def queens(n: Int): Set[List[Int]] = {

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queenRows = (row - 1 to 0 by -1) zip queens
    queenRows forall {
      case (r, c) => col != c && math.abs(col -c) != row - r
    }
  }
  def placeQueens(k: Int): Set[List[Int]] = {
    if(k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k-1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }

  placeQueens(n)
}

def show(queens: List[Int]):String = {
  val lines = for(col <- queens.reverse) yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

queens(4) map show mkString "\n"

