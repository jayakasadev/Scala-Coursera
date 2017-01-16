/*
implementing merge sort here
O(nlgn) is also the best case
 */

def mergeSort(in: List[Int]): List[Int] = {
  val split = in.length / 2
  if(split == 0) in
  else {

    val (r, l) = in splitAt(split)
    mergePatternMatch(mergeSort(r), mergeSort(l))
  }
}

def mergePatternMatch(left: List[Int], right: List[Int]): List[Int] = left match {
  case Nil => right
  case x :: xs => right match {
    case Nil => left
    case y :: ys =>
      if(x < y) x :: mergePatternMatch(xs, right)
      else y :: mergePatternMatch(left, ys)
  }
}

mergeSort(List(2, 3, 5, 1, 3, 2, 5, 6, 7))

mergeSort(List(33,44,22,-10,99))
mergeSort(List())

/*
splitAt function on lists returns 2 sublists in a pair
  up to n => left
  from n to end => right

pairs in scala
 */

//type is (String, Int)
val pair = ("Hello", 2)
val (name, value) = pair
// name is Hello
// value is 2
//this works analogously for tuples with more than 2 elements

/*
tuple type (T1, ... Tn) is an abbreviation for the parameterized type scala.Tuplen[T1, ..Tn]

tuple expression (e1, ..., en) is equivalent to function application scala.Tuplen(e1, ..en])

tuple pattern (p1, .., pn) is equivalent to constructor patten scala.Tuplen(p1, ..pn)
 */

def mergeSort2(in: List[Int]): List[Int] = {
  val split = in.length / 2
  if(split == 0) in
  else {

    val (r, l) = in splitAt(split)
    mergeTuple(mergeSort2(r), mergeSort2(l))
  }
}

def mergeTuple(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
  case (Nil, right) => right
  case (left, Nil) => left
  case (leftH :: leftT, rightH :: rightT) =>
    if(leftH < rightH) leftH :: mergeTuple(leftT, right)
    else rightH :: mergeTuple(left, rightT)
}

mergeSort2(List(2, 3, 5, 1, 3, 2, 5, 6, 7))

mergeSort2(List(33,44,22,-10,99))
mergeSort2(List())