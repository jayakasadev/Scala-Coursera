/*
classification and access methods:
  quadratic explosion

type tests and casts:
  unsafe, low-level

object-orientated decomposition:
  does not always work, need to touch all classes to add a new method

the sole purpose of test and accessor functions is to reverse the construction process:
  which subclass was used
  what were the arguments of the constructor

  functional languages automate this because it is so common
    done via pattern matching

case class
  definition is similar to normal class definition
    except that is it preceded by modifier case
 */

/*
Scala compiler implicitly defines companion objects with apply methods

object Number{
  def apply(n: Int) = new Number(n)
}

object Sum{
  def apply(left: Expr, right: Expr) = new Sum(left, right)
}

so we can write Number(1) instead of new Number(1)

pattern matching is a generalization of switch from C/Java to class heirarchies
expresses in scala with keyword match
 */

trait Expr{

  def eval: Int = this match {
    case Number(n) => n
    case Sum(left, right) => left.eval + right.eval
    case Prod(left, right) => left.eval * right.eval
  }

  def show:String = this match {
    case Number(n) => "" + n
    case Sum(left, right) => "(" + left.show + " + " + right.show + ")"
    case Prod(left, right) => "(" + left.show + " * " + right.show + ")"
    case Var(variable) => variable
  }
}

//the following are concrete subclasses
case class Number(n: Int) extends Expr

case class Sum(left: Expr, right: Expr) extends Expr

case class Prod(left: Expr, right: Expr) extends Expr

case class Var(variable: String) extends Expr

//possible to define the evaluation function as a method of the base trait
/*
def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(left, right) => eval(left) + eval(right)
  case Prod(left, right) => eval(left) * eval(right)
}
eval(Number(2))
eval(Sum(Number(2), Number(3)))
eval(Prod(Number(2), Number(3)))
*/

Number(2).eval
Sum(Number(2), Number(3)).eval
Prod(Number(2), Number(3)).eval

Number(2).show
Sum(Number(2), Number(3)).show
Prod(Number(2), Number(3)).show
Prod(Var("x"), Var("y")).show
/*
match is followed by a sequence of cases, pat => expr

each case associates an expression expr with a patter pat

A MatchError exception is thrown is no pattern matches the value of the selector

Patterns are constructed from
  constructors
    Number, Sum
  variables
    n, left, right
  wildcard patterns
    _
  constants
    1, true

variables always begin with lowercase letters

same variable name can only appear once in a pattern
  Sum(x, x) is illegal

names of constants begin with a capital letter
  with the exception of the reserved words
    null true false


 */

