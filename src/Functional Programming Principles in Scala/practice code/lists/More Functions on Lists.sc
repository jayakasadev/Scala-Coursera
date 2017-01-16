import scala.annotation.tailrec

/*
Sublists and element access

xs.length             -> # of elements in xs
xs.last               -> last element, exception if empty
xs.init               -> list consisting of all elements in xs except last one, exception if xs is empty
xs take n             -> list of first n elements of xs, or xs itself if xs is shorter than n
xs drop n             -> rest of collection after taking n elements
xs(n) or xs apply n   -> elements of xs at index n

xs ++ ys              -> list containing all elements of xs followed by ys
xs.reverse            -> list of xs elements in reverse order
xs updated (n, x)     -> list containing all elements of xs except at index n where it has x

xs indefOf x          -> index of first element in xs to match x if it exists else -1
xs contains x         -> same as xs contains x >= 0

complexity of head is constant time

complexity of last is n because you will have to go through each element
 */

@tailrec
def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(xs)
}

// O(n)
def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

//O(n)
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

//O(n * n) ==> n concats * n reverse()
def reverse[T](xs: List[T]): List[T] = xs match{
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

//O(n)
def removeAt[T](n: Int, xs: List[T]): List[T] = {
  //if(n == 0) xs.tail
  //else xs.head :: removeAt(n-1, xs.tail)
  (xs take n) ::: (xs drop n+1)
}

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def flatten[T](xs: List[T]): List[T] = xs match{
  case Nil => Nil
  //case y :: Nil => List(y)
  case y :: ys => (y match {
    case l: List[T] => flatten(l)
    case i => List(i)
  }) ++ flatten(ys)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))