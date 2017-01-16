/*
Map[Key, Value] extends the collections type Iterable[Key, Value] of ley/value type
support the same collection operations as iterables

can write key -> value or (key, value)

also extends the function type Key => Value
  maps can be used anywhere a function is
 */

class Poly(val input: Map[Int, Double]) {

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = input withDefaultValue(0.0)
  //def +(right: Poly): Poly = new Poly(terms ++ (right.terms map adjust))
  def +(right: Poly): Poly = new Poly((right.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coef) = term
    terms + (exp -> (coef + terms(exp)))
  }


  def adjust(term: (Int, Double)): (Int, Double) = {
    val(exp, coef) = term
    terms.get(exp) match {
      case Some(coef2) => (exp , coef + coef2)
      case None => (exp , coef)
    }
  }
  override def toString = (for((exp, coef) <- terms.toList.sorted.reverse) yield coef + "^" + exp) mkString " + "
}

val p1 = new Poly((1, 2.0), (2, 3.0))
val p2 = new Poly(Map((1, 2.0), (2, 3.0)))
p1 + p2

/*
maps are partial functions
map(key) can throw an exception if key was not added

use withDefaultValue to make map a complete function
 */

p1.terms(7)
