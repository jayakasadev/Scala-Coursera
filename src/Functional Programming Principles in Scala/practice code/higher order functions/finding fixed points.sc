import math.abs

/**
  * a number is a fixed point of a function f if f(x) = x
  *
  * for some functions f, we can locate the fixed points by starting with an initial
  * estimate and then applying f in a repetitive way
  *   x, f(x), f(f(x)), f(f(f(x))), ...
  * until the value does not vary anymore or change is sufficiently small
  *
  * think numerical analysis fixed point approximation and convergence
  *
  *
  */

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  abs((x - y)/ x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if(isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1+ x/2)(1)

/**
  * Square Root
  *
  * sqrt(x) = number y such that y * y = x
  * or
  * sqrt(x) = number y such that y = x / y
  *
  * this is a fixed point of the function (y => x / y)
  *
  * you will need to control the oscillations during the approximation process
  *
  * you can do this by preventing the estimation from varying too much
  *   average successive values of the original sequence
  *   y => (y + x / y) / 2
  */

def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
sqrt(2)

// general version of the above function
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

//this is a function that takes a function to another function
def sqrt2(x:Double): Double = fixedPoint(averageDamp(y => x / y))(1)
sqrt2(2)