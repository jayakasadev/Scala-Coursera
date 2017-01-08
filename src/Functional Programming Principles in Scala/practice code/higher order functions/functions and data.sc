/*
Types in Scala:
  Numeric
    Int, Double( and Byte Short Char Long Float)
  Boolean
    True False
  String
  function
 */

/*
Classes
  defines a new TYPE named Rational
  defines a CONSTRUCTOR Rational to create elements of this
  type

Scala keeps the names of type and values in different
namespaces
This way there is no conflict between two definitions
of Rational

We call the elements of a class type OBJECTS
you can create objects just like in java
 */

class Rational(x: Int, y: Int){
  //def are for functions and lazy variables
  //var are mutable values
  var numerator = x
  var denominator = y

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
  def makeRationalString: String = numerator + "/" + denominator

  override def toString: String = numerator + "/" + denominator
}

//a new rational object
//val are final variables
val rational1 = new Rational(1, 2)
val rational2 = new Rational(3, 4)

// can access the class vars with the dot
// operator
rational1.numerator
rational1.denominator

/*
you can package functions operating on a data abstraction in the data abstraction itself
these are called METHODS


 */

rational1.makeRationalString
rational1.add(rational2).makeRationalString
println("To String Method " + rational1)
rational2.negate
rational2.negate
rational1.subtract(rational2).makeRationalString