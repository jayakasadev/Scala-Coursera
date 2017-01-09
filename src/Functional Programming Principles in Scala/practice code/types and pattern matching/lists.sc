/*
list is a fundamental data structure in FP

lists are immutable
  elements in a list cannot be changed
lists are recursive
  arrays are flat

like arrays
  lists are homogeneous
    elements of a list must all have the same type

type of a list with elements of type T is written scala.List[T] or List[T]

all lists are constructed from
  empty list Nil
  construction operation ::
    called cons

    x :: xs
      gives a new list with the first element x followed by the elements of xs
 */

val fruit = List("apples", "oranges")
val nums = List(1, 2, 3, 4)
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
val empty = List()

val x = "apples" :: ("oranges" :: Nil)
val n = 1 :: (2 :: (3 :: Nil))

/*
operators ending in : associate to the right

A :: B :: C is interpretted as A :: (B :: C)
 */

val y = 1 :: 2 :: 3 :: 4 :: Nil

/*
operators ending in : are also different in that tey are seen as method calls of the right-hand operand

so y is equivalent to
  Nil.::(4).::(3).::(2).::(1)

:: is a prepend operation

all operations on lists can be expressed in terms of the following 3 operations
  head
    first element in list
  tail
    list composed of all elements except the first
  isEmpty
    true if empty, else false

these operations are defined as methods of objects of type list
 */

y.head
y.tail

/*
it is posible to decompose lists with pattern matching
  Nil
    Nil constant
  p :: ps
    pattern that matches a list with a head matching p and a tail matching ps
  List(p1, ..., pn)
    same as p1 :: ... :: pn :: Nil

x :: y :: List(xs, ys) :: zs
  the length of the above list is >= 3
  x, y, and List(xs, ys) are 3 elements
  zs can be another value or nil
  so there may be 3 or more elements in the above list
 */

// standard way to decompose a list
//pay attention to how the lists are decomposed and processed
// following algorithm is n * n
def insertsort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, insertsort(ys))
}

def insert(x:Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}
