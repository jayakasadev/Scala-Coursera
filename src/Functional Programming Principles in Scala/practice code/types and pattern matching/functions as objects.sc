import scala.Predef.Function

/*
Scala numeric types and Boolean type can be implemented like normal classes

so can functions

function A => B is just an abbreviation for the cllass scala.Function1[A, B] which is
defined as follows

functions are objects with apply methods

traits for Function2...Function22 which take more parameters
  max of 22 params in Function22
 */

trait Function[A, B]{
  def apply(x: A): B
}

/*
anonymous function
  (x: int) => x * x
the above can be expanded to
 */

class AnonFun extends Function1[Int, Int]{
  def apply(x: Int) = x * x
}

new AnonFun apply(7)

//anonymous function
val g = (x: Int) => x * x

g(7)

//OO translation
val f = new Function1[Int, Int] {
  def apply(x: Int) = x * x

}
f.apply(7)
f(8)


trait List[T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
}

class Nil[T] extends List[T]{
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

object List{
  //List(1, 2) = List.apply(1,2)
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil[T]))
  //List(1)
  def apply[T](x: T): List[T] = new Cons[T](x, new Nil[T])
  //List()
  def apply[T]: List[T] = new Nil[T]
}
List(1, 2)
List(1)
List