/*
Parameterize mergesort so it works for lists other than just List[Int]
 */

def mergeSort[T](in: List[T])(lt: (T, T) => Boolean): List[T] = {
  val split = in.length / 2
  if (split == 0) in
  else{
    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, right) => right
      case (left, Nil) => left
      case (x:: xs, y :: ys) =>
        if (lt(x, y)) x :: merge(xs, right)
        else y :: merge(left, ys)
    }

    val (left, right) = in splitAt split
    merge(mergeSort(left)(lt), mergeSort(right)(lt))
  }
}


val num = List(2.1, 3.1, 4.0, 1.4, .4, .6, 1.2, 4.8, .6, 2.3, .34, 10, -1.2)
val lt = (x: Double, y: Double) => x < y
mergeSort(num)(lt)

/*
There is already a predefined class in standard library that represents orderings

  scala.math.Ordering[T]

provides ways to compare elements of type T
so instead of parameterizing with the lt operation directly, we could parameterize with Ordering instead

can avoid passing ord values
  make ord an implicit parameter
  compiler will figure out the right implicit to pass based on the demanded type
 */

def mergeSort2[T](in: List[T])(implicit ord: Ordering[T]): List[T] = {
  val split = in.length / 2
  if (split == 0) in
  else{
    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, right) => right
      case (left, Nil) => left
      case (x:: xs, y :: ys) =>
        if (ord.lt(x, y)) x :: merge(xs, right)
        else y :: merge(left, ys)
    }

    val (left, right) = in splitAt split
    merge(mergeSort2(left), mergeSort2(right))
  }
}

//val num = List(2.1, 3.1, 4.0, 1.4, .4, .6, 1.2, 4.8, .6, 2.3, .34, 10, -1.2)
//val lt = (x: Double, y: Double) => x < y
mergeSort2(num)

/*
rules for implicit parameters
  compiler will search an implicit definition that
    is marked implicit
    has a compatible type T
    is visible at point of function call
    defined in a companion object associated with T

if there is a single defition
  will take as actual argument for implicit parameter
  else error


 */