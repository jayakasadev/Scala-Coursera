/*
wild card import
import package._
imports all in the package

import package.(A, B)
import A and B from package

import package.A
import A from package

In Java and Scala, a class can only have one superclass

A trait is the Scala version of interfaces
A trait lets you inherit from many different supertypes

A trait is declared just like an abstract class

cannot have value parameters like an abstract class

traits resemble interfaces in Java, but are more powerful
because they can contain fields and concrete methods

 */

trait Planar{
  def height: Int
  def width: Int
  def surface = height * width
}

/*
Any : base of all types
  Methods:
    ==
    !=
    equals
    hashCode
    toString

AnyRef: base type of all reference Objects
  Alias of 'java.lang.Object'

AnyVal: base type of all primitive types

Nothing is at the bottom og he Scala type heirarchy
  subtype of every other type
  no value of type Nothing

  uses:
    signal abnormal termination
      gives you something to return
    as an element type of empty collections
      set of no elements = set of Nothing


Scala exception handling is similar to Java's

  throw Exc
    aborts evaluation with the exception Exc
    type of this expression is Nothing

 */

def error(msg: String) = throw new Error(msg)
//error("Problem Boss")

/*
null is a subtype of all reference types
type of null is Null
\
 */

val x = null
//val y: Int = null
