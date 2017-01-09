trait Expr{
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr

  //3rd option for eval
  //have to define eval for all subclasses
  def eval: Int
}

class Number(n: Int) extends Expr{
  def isNumber: Boolean = true
  def isSum: Boolean = false
  def numValue: Int = n
  def leftOp: Expr = throw new Error("Numer.leftOp")
  def rightOp: Expr = throw new Error("Numer.rightOp")
  def eval: Int = n
}

class Sum(left: Expr, right: Expr) extends Expr{
  def isNumber: Boolean = false
  def isSum: Boolean = true
  def numValue: Int = throw new Error("Sum.numValue")
  def leftOp: Expr = left
  def rightOp: Expr = right
  def eval: Int = left.eval + right.eval
}


def eval(e: Expr): Int = {
  if(e.isNumber) e.numValue
  else if(e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression" + e)
}

eval(new Sum(new Number(3), new Number(8)))

/*
gets tedious when you want to add methods for classification and access to all classes

To integrate a class Prod and Var into the heirarchy, will need to add 25 new method definitions
  including method definitions in Prod and Var
  not counting methods already in the classes

  a "hacky" solution could use type tests and type cats

    def isInstanceOf[T]: Boolean // checks whether this object's type conforms to T
    def asInstanceOf[T]: T// treats this object as an instance of type T
    //throws ClassCastException if it is not

    like Java's instanceof operator

    this is discouraged in scala because there are better options
 */



//better solution
/*
+
  no need for classification methods, access methods only for classes where the value is defined
-
  low-level and potentially unsafe
    unsafe because when you do a type cast, you do not know at runtime whether the cast will succeed
 */
def eval2(e: Expr): Int = {
  if(e.isInstanceOf[Number]) e.asInstanceOf[Number].numValue
  else if(e.isInstanceOf[Sum]){
    eval2(e.asInstanceOf[Sum].leftOp) + eval2(e.asInstanceOf[Sum].rightOp)
  }
  else throw new Error("Unknown expression " + e)
}

eval2(new Sum(new Number(3), new Number(8)))

/*
a * b + a * c -> a * (b + c)
  this is a non-local simplification
  cannot be encapsulated in the method of a single object

  back to square one

  solution is pattern matching
 */

