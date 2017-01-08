def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero : Int)(a: Int, b:Int): Int =
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

def product(f: Int => Int)(a: Int, b: Int): Int =
  if(a > b) 1
  else f(a) * product(f)(a+1, b)

def mRProduct(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

def factorialP(n: Int): Int =
  product(x => x)(1, n)

def factorialMR(n: Int): Int =
  mRProduct(x => x)(1, n)

factorialP(5)
factorialMR(5)