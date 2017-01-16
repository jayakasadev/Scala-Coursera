/*
reduceLeft can only be applied to nonempty lists
it is defined in terms of a more general function
  foldLeft

foldLeft takes an accumulator as an additional parameter
which is returned when foldLeft is called on an empty list

 */
def sum1(list: List[Int]): Int = (0 :: list) reduceLeft(_ + _)
def product1(list: List[Int]): Int = (1 :: list) reduceLeft(_ * _)

def list = List(4, 2, 3)
sum1(list)
product1(list)

def sum2(list: List[Int]): Int = (list foldLeft 0)(_ + _)
def product2(list: List[Int]): Int = (list foldLeft 1) (_ * _)

sum2(List())
product2(List())
