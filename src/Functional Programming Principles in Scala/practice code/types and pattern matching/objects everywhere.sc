/*
Peano Numbers
 */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("0.predecessor")
  def successor: Nat = throw new Error("0.successor")
  def +(that: Nat): Nat  = that
  def -(that: Nat): Nat = if(that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat{
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = throw new Error("Succ.successor")
  def +(that: Nat): Nat  = new Succ(n+that)
  def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor
}