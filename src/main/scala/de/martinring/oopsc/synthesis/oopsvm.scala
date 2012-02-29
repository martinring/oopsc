package de.martinring.oopsc.synthesis.oopsvm
import scala.collection.immutable.SortedSet

/** Code-Composition Monad */
sealed trait Code[+T] {
  import Code._
  def apply(s: AssemblerState): (T,AssemblerState)
  def map[U](f: T => U): Code[U] = code { 
    case s =>
      val (t,s2) = apply(s)
      (f(t),s2)
  }
  def flatMap[U](f: T => Code[U]): Code[U] = code {
    case s =>
      val (t,s2) = apply(s)
      f(t)(s2)
  }
  def >>=[T] = flatMap[T] _
  def >>[T] (f: Code[T]): Code[T] = flatMap(_ => f)  
  def run: List[Line] = apply(AssemblerState.initial)._2.instructions
}

object Code {
  def code[T](f: AssemblerState => (T,AssemblerState)): Code[T] = new Code[T] {
    def apply(s: AssemblerState) = f(s)
  } 
  
  implicit def instruction(line: Line): Code[Unit] = code( s => ((),s.copy(instructions = s.instructions :+ line)) )
   
  
  def variable: Code[R] = code( s => (s.registers.head,s.copy(registers = s.registers.tail)) )
  
  def variable(name: String): Code[R] = code( s => 
    s.names.get(name) match {
      case None => (s.registers.head, s.copy(registers = s.registers.tail, names = s.names.updated(name,s.registers.head)))
      case Some(r) => (r, s)
    })
  
  def variable[T](f: R => Code[T]): Code[T] = for {
    r <- variable
    t <- f(r)
    _ <- free(r)
  } yield t
  
  def free(r: R): Code[Unit] = code( s => ((),s.copy(registers = s.registers + r)) )
  
  def just[T](t: T): Code[T] = code(s => (t,s))    
  
  def goto(r: Value): Code[Unit] = R0 := r
  
  def read: Value = result(r => SYS(0,r.n))  
  def write(v: Value): Code[Unit] = v.use(r => SYS(1,r.n))
  
  def result(f: R => Code[Unit]): Value = new Value {
    def assignTo(r: R): Code[Unit] = f(r)
  }
  
  def sequence(stuff: List[Code[Unit]]): Code[Unit] = stuff.foldLeft[Code[Unit]](just(())){ case (a,b) => (a >> b): Code[Unit] }  
  
  def lines(lines: Code[Unit]*): Code[Unit] = sequence(lines.toList)
  
  def when (cond: Value) = new { 
    def goto (target: Literal) = cond.use(c => JPC(c,target))
  }
    
  implicit def intLiteral(v: Int): Literal = IntLiteral(v)
  implicit def labelLiteral(v: Label): Literal = LabelLiteral(v)  
  implicit def useAsLValue(l: Code[R]): LValue = new LValue {
    def assignTo(r: R): Code[Unit] = l >>= (x => r := x)
    def :=(v: Value): Code[Unit] = l >>= (x => x := v)
    override def use(f: R => Code[Unit]) = l >>= f
    override def unary_~  = {
      val self = this
      new LValue {
        def assignTo(that: R) = l >>= (x => MRM(that, Address(x)))
        def :=(that: Value) = l >>= (x => that.use( MMR(Address(x),_) ))
      }
    }
  }
}

import Code._

sealed trait Value {
  def assignTo(r: R): Code[Unit]
  def use(f: R => Code[Unit]): Code[Unit] = { variable( temp => assignTo(temp) >> f(temp) ) }
  def + (that: Value) = result(r => (r := this) >> (that.use(that => ADD(r, that))))  
  def - (that: Value) = result(r => (r := this) >> (that.use(that => SUB(r, that))))
  def * (that: Value) = result(r => (r := this) >> (that.use(that => MUL(r, that))))
  def / (that: Value) = result(r => (r := this) >> (that.use(that => DIV(r, that))))
  def % (that: Value) = result(r => (r := this) >> (that.use(that => MOD(r, that))))
  def & (that: Value) = result(r => (r := this) >> (that.use(that => AND(r, that))))
  def | (that: Value) = result(r => (r := this) >> (that.use(that => OR(r, that))))
  def ^ (that: Value) = result(r => (r := this) >> (that.use(that => XOR(r, that))))
  def === (that: Value) = result(r => (r := this) >> (that match {
    case IntLiteral(0) => ISZ(r,r)
    case v => (r -= that) >> ISZ(r,r) }))
  def > (that: Value) = result ( r => (r := this) >> (that match {
    case IntLiteral(0) => ISP(r,r)
    case v => (r -= that) >> ISP(r,r) }))
  def < (that: Value) = result ( r => (r := this) >> (that match {
    case IntLiteral(0) => ISN(r,r)
    case v => (r -= that) >> ISN(r,r) }))
  def >= (that: Value) = result ( r => (r := this < that) >> (r := r^1) )
  def <= (that: Value) = result ( r => (r := this > that) >> (r := r^1) )    
  def unary_~ : LValue = {    
    val self = this
    new LValue {    
      def assignTo(that: R) = (that := self) >> (MRM(that,Address(that)))
      def :=(that: Value) = self.use( r => that.use( MMR(Address(r),_) ) )
    }
  }
  def unary_! = result(r => (r := this) >> (r := r === 0) )
  def unary_- = result(r => variable(t => (r := 0) >> assignTo(t) >> (r -= t)))
}

sealed trait LValue extends Value {
  def :=(v: Value): Code[Unit]
  def +=(v: Value) = this := this + v
  def -=(v: Value) = this := this - v
  def *=(v: Value) = this := this * v
  def /=(v: Value) = this := this / v
  def %=(v: Value) = this := this % v
  def &=(v: Value) = this := this & v
  def |=(v: Value) = this := this | v
  def ^=(v: Value) = this := this ^ v
}

sealed abstract class R(val n: Int) extends LValue {  
  override def toString = "R"+n
  def assignTo(r: R) =  (if (r == this) just(()) else MRR(r,this))
  override def use(f: R => Code[Unit]) = f(this)  
  def :=(v: Value) = v assignTo this
  override def unary_~  = {
    val self = this
    new LValue {
      def assignTo(that: R) = MRM(that, Address(self))
      def :=(that: Value) = that.use( MMR(Address(self),_) )
    }
  }
}

object R0 extends R(0)
object R1 extends R(1)
object R2 extends R(2)
object R3 extends R(3)
object R4 extends R(4)
object R5 extends R(5)
object R6 extends R(6)
object R7 extends R(7)

case class Address(r: R) { 
  override def toString = "(%s)" format r  
}

case class AssemblerState(instructions: List[Line], 
                          registers: SortedSet[R],
                          names: Map[String,R])

object AssemblerState {
  val initial = AssemblerState(
    List(MRI(R1,1)), 
    SortedSet(R2,R3,R4,R5,R6,R7)(Ordering.by(_.n)),
    Map.empty)
}

sealed trait Line {
  private var comment: Option[String] = None
  
  def apply(s: String): Line = {
    comment = Some(s)
    return this
  }
  
  override def toString = super.toString + comment.map("; " + _).getOrElse("")                          
}

case class Label(v: String) extends Line {
  override def toString = v+":" 
  def assignTo(r: R) = r := LabelLiteral(this)
}

sealed trait Literal extends Value { def assignTo(r: R): Code[Unit] = MRI(r, this) }

case class IntLiteral(v: Int) extends Literal { 
  override def use(f: R => Code[Unit]): Code[Unit] = v match {
    case 1 => f(R1)
    case _ => super.use(f)
  }
  override def toString = v.toString 
}
case class LabelLiteral(v: Label) extends Literal { override def toString = v.v }

sealed abstract class Instruction(name: String, args: Any*) extends Line {
  override def toString = name + " "  + args.mkString(", ")
}
case class MRI (Rx: R, n: Literal) extends Instruction("MRI",Rx,n)
case class MRR (Rx: R, Ry: R) extends Instruction("MRR",Rx,Ry)
case class MRM (Rx: R, s: Address) extends Instruction("MRM",Rx,s)
case class MMR (s: Address, Ry: R) extends Instruction("MMR",s,Ry)
case class ADD (Rx: R, Ry: R) extends Instruction("ADD",Rx,Ry)
case class SUB (Rx: R, Ry: R) extends Instruction("SUB",Rx,Ry)
case class MUL (Rx: R, Ry: R) extends Instruction("MUL",Rx,Ry)
case class DIV (Rx: R, Ry: R) extends Instruction("DIV",Rx,Ry)
case class MOD (Rx: R, Ry: R) extends Instruction("MOD",Rx,Ry)
case class AND (Rx: R, Ry: R) extends Instruction("AND",Rx,Ry)
case class OR (Rx: R, Ry: R) extends Instruction("OR",Rx,Ry)
case class XOR (Rx: R, Ry: R) extends Instruction("XOR",Rx,Ry)
case class ISZ (Rx: R, Ry: R) extends Instruction("ISZ",Rx,Ry)
case class ISP (Rx: R, Ry: R) extends Instruction("ISP",Rx,Ry)
case class ISN (Rx: R, Ry: R) extends Instruction("ISN",Rx,Ry)
case class JPC (Rx: R, n: Literal) extends Instruction("JPC",Rx,n)
case class SYS (f: Literal, n: Literal) extends Instruction("SYS",f,n)
case class DAT (l: Int, n: Literal) extends Instruction("DAT",l,n)

case class Stack(name: String, size: Int) {
  val start: Label = Label(name)
  
  val position: Code[R] = variable(name)  
  
  val initialize: Code[Unit] = position >>= (r => r := start)
  val allocate: Code[Unit] = start >> DAT(size, 0)
  
  def push(r: Value): Code[Unit] = (position += 1) >> (~position := r)
  def pop: Value = result ( r => (r := ~position) >> (position -= 1) )
  
  def top: LValue = new LValue {
    def assignTo(r: R) = r := ~position
    def :=(v: Value) = ~position := v
  }
}

case class Heap(name: String, size: Int) {
  val start: Label = Label(name)  
  
  val position = MemoryVar(name + "_pointer")    
  
  val allocate: Code[Unit] = position.allocate(start) >> start >> DAT(size, 0)    
}

case class MemoryVar(name: String) extends LValue {
  val position: Label = Label(name)  
  
  def allocate(value: Literal): Code[Unit] = position >> DAT(1, value)  
  
  def assignTo(r: R) = (r := ~position)
  def :=(v: Value) = (~position := v)
  override def use(f: R => Code[Unit]) = variable ( p => (p := ~position) >> f(p) )
  
  def modify(f: R => Code[Unit]) = variable( p => (p := position) >> variable( x => (x := ~p) >> f(p) >> (~p := x) ) )
}