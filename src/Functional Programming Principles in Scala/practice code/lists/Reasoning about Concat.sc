def factorial(n: Int): Int = {
  if (n == 0) 1
  else n * factorial(n - 1)
}

def concat[T](left: List[T], right: List[T]): List[T] = left match {
  case List() => right
  case x :: xs => x :: concat(xs, right)
}