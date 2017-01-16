def map[T](list: List[T])(f: T => T): List[T] = list match {
  case List() => List()
  case x :: xs => f(x) :: map(xs)(f)
}

val f = (x: Int) => x * x
map(List(2, 3, 4))(f)
map(Nil)(f)

def filter[T](list: List[T])(f: T => Boolean): List[T] = list match {
  case List() => List()
  case x :: xs => if(f(x)) x :: filter(xs)(f) else filter(xs)(f)
}

val g = (x: Int) => x > 3
filter(List(2, 3, 4))(g)

/*
xs filterNot f    => filter(x => !f(x))

xs partition f    => same as (xs filter f, xs filterNot f), computed in a single traversal of xs

xs takeWhile f    => longest prefix of xs consisting of elements that all satisfy the predicate f

xs dropWhile f    => remainder of the list xs after any leading elements satisfying f are removed

xs span f         => same as (xs takeWhile f, xs dropWhile f) but computed in a single traversal

 */

val temp = List(2, 1,3, 4,13,4,1,23,2,3,122,1,1,23,1,1,2,212,3,2,2,12,3,5,1)
temp partition(x => x > 100)
temp span(x => x > 100)

def pack[T](list: List[T]): List[List[T]] = list match {
  case Nil => Nil
  case x :: xs =>
    val (left, right) = list span(y => x == y)
    left :: pack(right)
}

val temp2 = List(1, 2,2,2,2,2,1,1,1,1,2,2,1,1,2,2,1,1,2,2,1,2)
val packed = pack(temp2)

def encodePacked[T](list: List[List[T]]): List[(T, Int)] = {
  list map(x => (x.head, x.length))
  //case Nil => Nil
  //case x :: xs => (x.head, x.length) :: encode(xs)
}

val encoded = encodePacked(packed)

def encode[T](list: List[T]): List[(T, Int)] = {
  pack(list).map(x => (x.head, x.length))
}

encode(temp2)
