import scala.util.control.NonFatal

/*
data structures with map and flatMap are called monads

A monad M is a parametric type M[T] with two operations:
  flatMap
  Map
The operations have to satisfy some laws

flatmap is commonly called bind

List is a monad with unit(x) = List(x)
Set is a monad with unit(x) = Set(x)
Option is a monad with unit(x) = Some(x)
Generator is a monad with unit(x) = single(x)

flatMap is an operation on each of these types
whereas unit in Scala is different for each Monad

map can be defined for every monad as a combination of flatMap and unit

To qualify as a monad, need to pass following rules:
  Associativity:
    (m flatMap f) flatMap g == m flatMap ((x => f(x) flatMap g))
    (x + y) + z = x + (y + z)

  Left Unit:
    unit(x) flatMap f = f(x)

  Right Unit:
    m flatMap unit == m
 */

trait M[T] {
  def flatMap[U](f: T => M[U]): M[U]
}

val trying = new Try{
  def apply[T](expr: => T): Try[T] = try Success(expr)
  catch{
    case NonFatal(ex) => Failure(ex)
  }

}

abstract class Try[+T]{
  //self => //alias for this

  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch {
      case NonFatal(x) => Failure(x)
    }
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => trying(f(x))
    case fail:Failure => fail
  }
}
case class Success[T](x: T) extends Try[T]
case class Failure(ex: Throwable) extends Try[Nothing]

trying(2 / 0)
trying(2 / 2)

def f(x: Int) = trying(x / 0)
def s(x: Int) = trying(x * 2)
/*
these are equivalent statements

(trying(2 / 2) map (x => f(x))) == (trying(2 / 2) flatMap (x => trying(f(x))))
 */

trying(2 / 2) map f
trying(2 / 2) flatMap (x => trying(f(x)))

/*
Testing to see if Try is a monad

Associativity:
  m flatMap f flatMap g == m flatMap (x => f(x) flatMap g)
 */
trying(2 / 2) flatMap (x => s(x)) flatMap(x => f(x))
trying(2 / 2) flatMap (x => s(x)) flatMap(x => f(x))

/*
Left Unit:
  unit(x) flatMap f = f(x)
*/
trying(2 / 2) flatMap(x => s(x))
s(2/2)

/*
Right Unit:
  m flatMap unit == m
 */

trying(2 / 2) flatMap(x => trying(x))
trying(2 / 2)

/*
Try is a Monad
 */