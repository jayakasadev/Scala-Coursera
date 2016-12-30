/**
  * The ability to choose different implementations of the data without affecting clients is called
  * data abstraction
  *
  * In scala, a class implicitly introduces a constructor called the primary constructor of the class
  *   takes the parameters of the class
  *   executes all statements in the class body like require statements
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

  /**
    * Require is a predefined function used to enforce preconditions on the caller of the function
    *
    * it takes a condition and an optional message String
    * if the condition passed to require is false, an IllegalArgumentException is thrown with the
    * given message
    *
    */
  // if y is zero -->
  // java.lang.IllegalArgumentException: requirement failed: Denominator must be nonzero
  require(y != 0, "Denominator must be nonzero")

  //this is an overloaded constructor that takes a single argument that calls the implicit constructor
  def this(x: Int) = this(x, 1)

  /*
  Assert is used to check the code of the function

  takes a condition and an optional message String as a parameter

  val x = sqrt(y)
  assert(x >= 0)

  failing an assert will throw an AssertionError
   */

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


  //original
  //def numerator = x / g
  //def denominator = y / g

  //to allow negation and value change
  var numerator = x / g
  var denominator = y / g

  //var numerator = x
  //var denominator = y

  /*
  //have to call the same function twice here
  //this can be advantageous if the functions numerator and denominator are called infrequently

  var numerator = x / gcd(x, y)
  var denominator = y / gcd(x, y)
  */

  //changing numerator and denominator to val can be advantageous is they are called often
  //but we cannot change it again after this
  //so negation would have to return a new object
  //val numerator = x / g
  //val denominator = y / g

  /**
    * Compare two Rationals for a less than comparison
    *
    * @param that
    * @return
    */
  def less(that: Rational): Boolean = numerator * that.denominator  < that.numerator * denominator

  /**
    * Method returns which Rational is larger
    *
    * @param that
    * @return
    */
  def max(that: Rational):Rational = if(this.less(that)) that else this

  /**
    * Method to add 2 rationals
    *
    * @param s
    * @return
    */
  def add(s: Rational): Rational =
    new Rational(numerator * s.denominator + s.numerator * denominator,
      denominator * s.denominator)

  /**
    * Method to negate a rational object
    * @return
    */
  def negate: String ={
    numerator = -numerator
    makeRationalString
  }

  def subtract(s: Rational) =
    new Rational(numerator * s.denominator - s.numerator * denominator,
      denominator * s.denominator)

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

x.numerator
x.denominator
x.subtract(y).subtract(z)
y.add(y)
x.less(y)
x.max(y)
y.max(z)
//val strange = new Rational(1, 0)
//strange.add(strange) // this gives java.lang.ArithmeticException: / by zero
new Rational(2)


/**
  * if you modify Rational so that rational numbers are kept unsimplified
  * internally, but set the simplification process to run when the numbers are
  * converted to strings, you will see the same behavior as before for small sizes
  * of denominators and nominators and small numbers of operations only
  *
  * best to normalize numbers as early as possible to avoid arithmetic overflows
  */