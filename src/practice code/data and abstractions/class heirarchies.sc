/*
abstract class

can contain members which are missing an implementation
cannot instantiate

very similar to Java abstract classes
 */
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

/*
binary tree

two types of tree: empty and nonempty

creates a persistent data structure
 */
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x:Int): Boolean = {
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else x == elem
  }

  def incl(x: Int): IntSet = {
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if(x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left + " " + elem + " " + right + "}"
}

/*
defining an object ==> defines a singleton

no other instances of Empty can be or need to be created

singleton objects are values
  so Empty evaluates to itself
 */
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other

  override def toString = "."
}

var tree1 = Empty incl 3
var tree1m = tree1 incl 2 incl 7 incl 6 incl 8 incl -1 incl -3 incl -9
var tree2 = Empty incl 2
var tree2m =  tree2 incl 12 incl 20 incl -8 incl 4 incl -2

var unionSet = tree1m union tree2m


object Hello {
  def main(args: Array[String]): Unit = {
    println("Hello")
  }
}
