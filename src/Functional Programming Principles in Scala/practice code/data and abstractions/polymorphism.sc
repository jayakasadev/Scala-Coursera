import java.util.NoSuchElementException

var l1 = List(1, 2, 3)
var l2 = List(List(true, true), List(3))

/*
creating a custom list that takes a type parameter T
 */
trait List[T]{
  def isEmpty(): Boolean
  def head(): T
  def tail(): List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty(): Boolean = false
}

class Nil[T] extends List[T]{
  def isEmpty() = true

  def head = throw new NoSuchElementException("Nil.head")

  def tail = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

// the type of each is inferred by the compiler
singleton(1)
singleton(true)

/*
type parameters do not affect evaluation in Scala

we can assume that all type parameters and type arguments are removed before
evaluating the program

called type erasure
 */

val list1 = new Cons(3, new Nil)
val list2 = new Cons(5, new Cons(4, new Cons(1, new Cons(2, list1))))

def nth[T](n: Int, xs: List[T]): T = {
  if(n < 0 || xs.isEmpty()) throw new IndexOutOfBoundsException
  else if(n == 0) xs.head()
  else nth(n - 1, xs.tail())
}

nth(2, list2)
// nth(-1, list2)