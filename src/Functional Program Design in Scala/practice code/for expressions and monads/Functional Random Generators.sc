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

