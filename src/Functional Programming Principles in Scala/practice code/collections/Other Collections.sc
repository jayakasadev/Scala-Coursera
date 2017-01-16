/*
Vector
  more evenly balanced access pattern than list
  immutable like lists
  analogous to lists
    support the same operations
      except ::
      alternatives
        x +: xs
          create a new vector with the leading element x followed by xs
        xs :+ x
          create a new vector with trailing element x preceded by xs
  note
    : always points to the sequence

 */
val nums = Vector(1, 2, 3, 4)
val str = Vector("Str", "ing")

/*
common base class of List and Vector is Seq
  class of sequences
  subclass of Iterable
arrays and strings support the same operations as seq and can implicitly be converted to sequences where needed
  cannot be subclasses of Seq because they come from Java
 */

val xs: Array[Int] = Array(1, 2, 3)
xs map (_*2)
val ys: String = "Hello World"
ys filter (_.isUpper)

/*
another simple kind of sequence is range
  represents a sequence of evenly spaced integers
  3 operators
    to      -> inclusive
    until   -> exclusive
    by      -> to determine step value

Ranges are represented as single objects with 3 fields
  lower bound, upper bound and step value
 */

val r: Range = 1 until 5
val s: Range = 1 to 5
1 to 10 by 3
6 to 1 by -2

/*
Sequence operations

xs exists p   => true if there exists an elements that satisfies p
xs forall p   => true if p(x) holds for all x in xs, else false
xs zip ys     => sequence of pairs drawn from corresponding elements of sequences xs and ys
xs.unzip      => splits a sequence of pairs xs into 2 sequences consisting of the first , respectively second halved of
                 all pairs
xs.flatMap f  => applies collection-valued function f to all elements of xs and concatenates the results
xs.sum        => sum of all elements in numeric collection
xs.product    => product of all elements in numeric collection
xs.max        => maximum of all elements in list (Ordering must exist)
xs.min        => min of all elements in collection
 */

ys exists(_.isUpper)
ys forall(_.isUpper)

//ys = "Hello World"
val pairs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) zip ys
pairs unzip

ys flatMap (List('.', _))

xs.sum
xs.product

/*
To list all combinations of numbers x and y here x is drawn from 1-M and y is drawn from 1-N
  (1 to M) flatMap (x => (1 to N) map (y => (x, y)))
 */

val M = 7
val N = 3
(1 to M) flatMap (x => (1 to N) map (y => (x, y)))

/*
computer scalar product of 2 vectors
 */

def scalarProduct(left: Vector[Double], right: Vector[Double]): Double = {
  (left zip right).map(z => z._1 * z._2).sum
}

//pattern matching version
def scalarProductPatternMatch(left: Vector[Double], right: Vector[Double]): Double = {
  (left zip right).map{case(x, y) => x * y}.sum
}
/*
{case p1 => e1 ... case pn => en} is equivalent to
  x => x match {case p1 => e1 ... case pn => en}
 */

/*
a number is prime if  the only divisors are 1 and itself
the following is a high level way to write a test for primality
valuing conciseness over efficiency
 */
def isPrime(n: Int): Boolean = (2 until n) forall(n % _ != 0)

isPrime(2)
isPrime(1)