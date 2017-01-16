/*
generate the sequence of all pairs (i, j) such that 1 <= j < i < n
filter the pairs for which i+j is prime

one natural way to generate the sequence of pairs is to
  generate all the integers i between 1 and n (excluded)
  for each integer i, generate the list of pairs (i, 1) ... (i, i-1)
 */
val n = 7
val p1 = (1 until n) map (x => (1 until x) map (y => (x, y)))

/*
IndexedSeq
  sequence that uses random access
 */

/*
generate pairs
  combine all the subsequences using foldRight with ++ or flatten
 */

val p2 = ((1 until n) map (x => (1 until x) map (y => (x, y)))).flatten

/*
useful law
  xs flatMap f = (xs map f).flatten
  p1 and p2 can be simplified to p3
 */
val p3 = (1 to n) flatMap (x => (1 to x) map (y => (x, y)))

//need this for getting the prime combos
def isPrime(n: Int): Boolean = (2 until n) forall(n % _ != 0)

//filter the pairs for which i+j is prime
val p4 = (1 to n) flatMap (x => (1 to x) map (y => (x, y))) filter (z => isPrime(z._1 + z._2))

/*
higher order functions (map, flatMap, filter) provide powerful constructs for manipulating lists
but sometimes the level of abstraction required by these functions make the program difficult to understand

for expression can help here
 */

val list = (1 to n).toList
list filter (isPrime(_)) map (x => x * x)
for (x <- list if isPrime(x)) yield x * x

/*
Syntax for For
  for (s) yield e

  s is a sequence of generators and filters
  e is an expression whose value is returned by an iteration

  generator is of form p <- e
    p = pattern
    e = expression whose value is a collection

  filter is or form f
    f = boolean expression

  sequence must start with a generator

  if there are several generators in the sequence, last generators vary faster than the first

  can use {s} to write filters and generators on multiple lines without a semicolon

 */

for {
  x <- 1 to n
  y <- 1 to x
  if(isPrime(x + y))
} yield (x, y)

def scalerProduct(left: List[Double], right: List[Double]): Double = {
  (for ((x, y) <- left zip right) yield x * y).sum
}

scalerProduct((1.0 to 7.0 by 1).toList, (1.0 to 8.0 by 1).toList)