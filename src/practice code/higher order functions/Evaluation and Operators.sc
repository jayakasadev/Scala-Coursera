/**
  * How is an instantiation of the class new C(a1, ...., an) evaluated?
  * The expression arguments a1,...an are evaluated like the arguments of a normal function
  * the resulting expression (new C(a1,...an)) is already a value
  *
  * how is new C(a1,...an).f(b1,...bn) evaluated?
  * the expression C(a1,...an).f(b1,...bn) is reqritten to:
  *   [b1/y1,...bn/yn][a1/x1,..,an/xn][new C(a1,...an)/this]b
  *
  *   there are 3 substitutions
  *     substitution of formal parameters y1,...,yn of the function f by the arguments b1,...bn
  *
  *     substitution of the formal parameters x1,..,xn of the class C by the class arguments a1,..,an
  *
  *     the substitution of the self reference this by the value of the object new C(a1,...an)
  *
  * new Rational(1,2).numerator
  *   --> [1/x, 2/y][][new Rational(1,2)/this]x
  *   = 1
  *
  * new Rational(1,2).less(new Rational(2, 3))
  *   --> [1/x, 2/y][new Rational(1,2)/that][new Rational(1,2)/this]
  *   = this.numerator * that.denominator < that.numerator * this.denominator
  *   = new Rational(1, 2).numerator * new Rational(2, 3).denominator <
  *     new Rational(2, 3).numerator * new Rational(1, 2).denominator
  *   = 1 * 3 < 2 * 2
  *   = true
  *
  * operators can be used as identifiers
  *   Alphanumeric
  *     starting with a letter followed by a sequence of alphanumeric characters
  *
  *   Symbolic
  *     starting with an operator symbol followed by more operator symbols
  *
  *   Underscore Character _ counts as a letter
  *
  *   Alphanumeric identifiers can also end in an underscore followed by some operator symbols
  *
  * Examples
  *   x1    *     +?%&      vector_++     counter_=
  *
  */

/**
  * Defined a Rational Class.
  * Will support simplification
  *
  * @param x
  * @param y
  */
class Rational(x: Int, y: Int){
  //def are for functions and lazy variables
  //var are mutable values

  // if y is zero -->
  // java.lang.IllegalArgumentException: requirement failed: Denominator must be nonzero
  require(y != 0, "Denominator must be nonzero")

  //this is an overloaded constructor that takes a single argument that calls the implicit constructor
  def this(x: Int) = this(x, 1)

  /*
  Method to simplify the output value
   */
  private def gcd(a: Int, b: Int): Int = {
    if(b == 0) a
    else gcd(b, a % b)
  }


  //saving the gcd in a value
  //private val g = gcd(x, y)
  private def g = gcd(x, y)


  //to allow negation and value change
  var numerator = x / g
  var denominator = y / g

  /**
    * Compare two Rationals for a less than comparison
    *
    * @param that
    * @return
    */
  def <(that: Rational): Boolean = numerator * that.denominator  < that.numerator * denominator

  /**
    * Method returns which Rational is larger
    *
    * @param that
    * @return
    */
  def >(that: Rational):Rational = if(this.<(that)) that else this

  /**
    * Method to add 2 rationals
    *
    * @param s
    * @return
    */
  def +(s: Rational): Rational =
    new Rational(numerator * s.denominator + s.numerator * denominator,
      denominator * s.denominator)

  /**
    * Method to negate a rational object
    * @return
    */
  //have to keep a spave between unary_- and the : for it to be a legal method name
  def unary_- : Rational = new Rational(-numerator, denominator)

  def -(s: Rational) = this + -s

  /**
    * Creates a String
    * @return
    */
  def makeRationalString: String = {
    //(numerator / g) + "/" + (denominator / g)
    numerator + "/" + denominator
  }

  override def toString: String = {
    //(numerator / g) + "/" + (denominator / g)
    numerator + "/" + denominator
  }
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x < y
x > y

x + y
x - y
x - y - z
-x

/*
a + b ^? c ?^ d less a ==> b | c

evaluates to

  ((a + b) ^? (c ?^ d)) less ((a ==> b) | c)
 */