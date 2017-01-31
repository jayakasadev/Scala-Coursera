import scala.util.Random

//trait to generate random values of type T
/*
trait Generator[+T]{
  val rand = new Random()
  def generate:T
}

val integers = new Generator[Int] {
  override def generate = rand.nextInt()
}
*/

trait Generator[+T]{
  self => //alias for this

  val rand = new Random()

  def generate:T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]):Generator[S] = new Generator[S] {
    override def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  override def generate = rand.nextInt()
}

// val booleans = for(x <- integers) yield (x > 0)
// expands to
val booleans = integers map (x => x > 0)

/*
def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
  x <- t
  y <- u
} yield (x, y)

translates to
*/
def pairs[T, U](t: Generator[T], u: Generator[U]) = {
  t flatMap (x => u map (y => (x, y)))
}

def g = integers.generate > 0

(1 until 10) map (x => println(g))


def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] = integers map (x => lo + x % (hi-lo))

def oneOf[T](xs: T*): Generator[T] = choose(0, xs.length) map(idx => xs(idx))


//generate a random list
def emptyLists = single(Nil)
def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

def lists: Generator[List[Int]] = if(booleans.generate) emptyLists else nonEmptyLists


trait Tree

case class Inner(left: Tree, right: Tree) extends Tree

case class Leaf(x: Int) extends Tree

def trees:Generator[Tree] = for{
  isEmpty <- booleans
  tree <- if(isEmpty) leafs else inners
} yield tree

def leafs:Generator[Leaf] = integers map (Leaf(_))

def inners: Generator[Inner] = for {
  left <- trees
  right <- trees
} yield Inner(left, right)

trees.generate

/*
unit tests:
  - come up with some test input to program functions and a postcondition
  - postcondition is property of expected result
  - verify the program satisfies the postcondition

random test inputs let you work without test inputs
 */
//random testing
def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  (0 until numTimes) map(x => {val value = g.generate; assert(test(value), " test failed for " + value)})
  println("passed " + numTimes + " random tests")
}

//the below test does not hold for empty lists
test(pairs(lists, lists)){
  case (x, y) => (x ++ y).length > x.length
}

/*
instead of writing test, write properties that are assumed to hold
this idea is implemented by ScalaCheck tool
 */