package idealized.scala

/**
  * Created by admin on 7/21/2016.
  */
abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T) : T

  def && (x: => Boolean) : Boolean = ifThenElse(x, False)
  def || (x: => Boolean) : Boolean = ifThenElse(True, x)
  def unary_! : Boolean = ifThenElse(False,True)
  def == (x:Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x:Boolean): Boolean = ifThenElse(x.unary_!, x)

  def < (x:Boolean): Boolean = ifThenElse(False, x)
}

abstract  class  Int {
  def + (that: Double) : Double
  def + (that: Float) : Float
  def + (that: Long) : Long
  def + (that: Int) : Int

  def << (cnt: Int) : Int

  def & (that: Int) : Int
  def & (that: Long) : Int

  def == (that: Double) : Boolean
  def == (that: Float) : Boolean
  def == (that: Long) : Boolean
  def == (that: Int) : Boolean
}

object False extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = e
}

object True extends Boolean {
  def ifThenElse[T](t: => T, e: => T) : T = t
}

// Peano numbers
abstract class  Nat {
  def isZero : Boolean
  def predecessor : Nat
  def successor : Nat = new Succ(this)
  def +(that: Nat) : Nat
  def -(that: Nat) : Nat
}

object Zero extends  Nat {
  def isZero : Boolean = True;
  def predecessor : Nat = throw new Error("0.pred")
  def +(that: Nat) : Nat = that
  def -(that: Nat) : Nat = if (that.isZero) this else throw  new Error("negative Number")
}

class Succ(n: Nat) extends Nat {
  def isZero : Boolean = False
  def predecessor : Nat = n
  def +(that: Nat) : Nat = new Succ(n + that)
  def -(that: Nat) : Nat = if (that.isZero) n else n - that.predecessor
}
