
/*
LowerBounds
[S >: NonEmpty ]
  limits the subclass
  S is a supertype of NonEmpty
  T is a subtype of S
  S can be any supertype of NonEmpty


UpperBounds
[T <: IntSet]
  limits the parent class
  T can be instantiated only to types that conform to IntSet
  T is a subtype of IntSet

Can combine these to further limit the inheritance
[S >: NonEmpty <: IntSet
  S is bounded below by NonEmpty and bounded above by IntSet

NonEmpty <: IntSet
List[NonEmpty] <: List[IntSet]
  list of NonEmpty is also a list of IntSet
  this is because NonEmpty is a special case of IntSet aka a subtype
  the two types are covariant
    their subtyping relationship variaes with the type parameter

Arrays

array of T elements in Java ==> T[]

array of T in scala ==> Array[T]

arrays in Java are covariant, so we can have NonEmpty[] <: IntSet[]
  this can be a problem at times

Java:
NonEmpty[] a = new NonEmpty[]{new NonEmpty(1, Empty, Empty)}
IntSet[] b = a
b[0] = Empty -----> this will give you a runtime error
NonEmpty s = a[0]

Scala:
val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
val b: Array[IntSet] = a ---> Type Error
b(0) = Empty
val s: NonEmpty = a(0)

Scala does not allow covariant types in Arrays

Liskov Substitution Principle: --> why a type can be a subtype of another
  If A <: B, then everything one can do with a value of type B one should also
  be able to do with a value of type A

  Let q(x) be a property provable about objects x of type V
  Then q(y) should be provable for objects y of type A where A <: B
 */

/*
Scala list is immutable --> covariant
Scala array is mutable -->  not covariant

Say C[T] is a parameterized type andA, B are types such that A <: B (A is the parent class of A)

there are 3 possible relationships between c[A] and C[B]
  C[A] <: C[B]                                    --> C is covariant
  C[A] >: C[B]                                    --> C is contravariant
  neither C[A] nor C[B] is a subtype os the other --> C is nonvariant

Scala lets you declare the variance of the type by annotating the type parameter

  class C[+A]{}   --> C is covariant
  class C[-A]{}   --> C is contravariant
  class C[A]{}    --> C is nonvariant


say you have 2 function types
  type A = IntSet => NonEmpty
  type B = NonEmpty => IntSet
  according to Liskov Substitution Principle ==> A <: B (A is a subtype of B)

If A2 <: A1 and B1 <: B2
  A1 => B1 <: A2 => B2
  a function that takes a function that takes a parent type of one and returns the child type of another is contraviant
  and can take the child of the parent and return the parent of the child

  functions in scala are contravariant in their argument types and covariant in their result type

*/

//revised definition of Function1 trait
// the parameter is contravaraint and the return type is covariant
trait Function1[-T, +U]{
  def apply(x: T): U
}

/*
Scala compiler will check that there are no problematic combinations when compiling a class with variance annotation

  covariant type parameters can only appear in method results
  contravariant type parameters can only appear in method parameters
  invariant type parameters can appear anywhere
 */
/*
consider adding a prepend method to List which prepends a given element, yielding a new list
 */
trait List[+T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  //prepend fails variance checking in the following implementation
  //def prepend(elem: T): List[T] = new Cons[T](elem, this)
  //taking a supertype parameter of type T
  def prepend[U >: T](elem: U): List[U] = new Cons[T](elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
}

/*
objects in scala cannot have type parameters because there is only one instance of them

Nil is a subtype fo a List of Nothing
 */
object Nil extends List[Nothing]{
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}


//list of String is expecting a subtype of List[String]
//currently the List[T] is nonvaraint
// val x: List[String] = Nil

//must add a + to List[T] to make it covariant
val x: List[String] = Nil

/*
compiler is right to throw out List with prepend because it violates the Liskov Substitution Principle

given List[IntSet]
  you can do xs.prepend(Empty)
given List[NonEmpty]
  ys.prepend(Empty) --> throws a type mismatch error

List[NonEmpty] cannot be a subtype of List[IntSet]

the solution is to use a lower bound
  def prepend[U >: T](elem: U): List[U] = new Cons[T](elem, this)

  this passes variance checks

    covaraint type parameers may appear in lower bounds of method type parameters

    contravariant type parameters may appear in upper bounds of method

def f(xs: List[NonEmpty], x: Empty) = xs prepend x

  returns a List[IntSet]

  Intset is supertype os Empty and NonEmpty
  we call the prepend method on a List[NonEmpty] --> T = NonEmpty
  prepend takes a type parameter that can be an arbitrary supertype of NonEmpty --> U is supertype of NonEmpty
  elem is of type Empty --> U = Empty
  since Empty is not the parent type of NonEmpty
    the type inferencer chooses the next higher type which is IntSet
    U is set to IntSet
  so the method ends up returning a List[IntSet]
 */

