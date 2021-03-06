package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x:Int) => x == elem
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (elem: Int) => s(elem) || t(elem)
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = (elem: Int) => s(elem) && t(elem)
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = (elem: Int) => s(elem) && !t(elem)
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = (elem: Int) => s(elem) && p(elem)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound) true
        else if (s(a) && !p(a)) false
        else iter(a+1)
      }
      iter(-bound)
    }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  /*
    orig: 1 2 3
    p: x => X > 2

    exists(orig, p) -> !forall(orig, x => !p(x))
      -> !(1 > 2) = True => False
      -> !(2 > 2) = True => False
      -> !(3 > 2) = False => True


    exists(orig, p) -> forall(orig, x => p(x))
      -> (1 > 2) = False
      Terminates here and does not continue
   */
    def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (elem:Int) => !p(elem))
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = (elem: Int) => exists(s, (x: Int) => f(x) == elem)
  // does not really modify the original Set
  //defines a new Set which applies the function to the original set values
  /*
    orig: 1 2 3
    map: f(1) f(2) f(3)

    so map(orig, x => x * x)(2) = false
    map(orig, x => x * x)(4) = true
  */
  /*
    def map(s: Set, f: Int => Int): Set = {
      def transform(elem: Int, mapped: Set): Set = {
        if(elem > bound) mapped
        if(s(elem)) transform(elem+1, union(mapped, singletonSet(f(elem))))
        else transform(elem+1, mapped)
      }
      transform(-bound, (x: Int) => false)
    }
  */
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
