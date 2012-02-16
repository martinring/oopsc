package de.martinring.oopsc.synthesis

import de.martinring.oopsc._
import scala.collection.mutable.Buffer

/**
 * Integrated DSL for generation of OOPSVM assembler code.
 */
trait Assembler {    
  var labelCounter = 0
  
  val instructions = Buffer[String]()
  
  implicit def instrToString(i: Instr) = i.toString
  
  var freeRegisters = List[R](R1,R2,R3,R4,R5,R6,R7)
  var usedRegisters: Map[Symbol,R] = Map()
  val one: R = 'one
  
  def force(r: R): R = {
    freeRegisters = freeRegisters.filter(_ != r)
    return r
  }
  
  'one := 1

  val error: String = "_error"
  
  def error(label: String) {
    R7 := label
    goto(error)
  }  
  
  def string(s: String) {
    ("\n"+s+"\n").toList.foreach { case x: Char => Instruction("DAT",1,x.toInt) }
    Instruction("DAT",1,0)    
  }  
  
  implicit def symToR(sym: Symbol): R = 
    usedRegisters.get(sym).getOrElse {      
      if (freeRegisters.size < 0) instructions foreach println
      require(freeRegisters.size > 0, sym.name + " can not be bound " + usedRegisters.mkString(", "))
      val r = freeRegisters.head
      instructions.append("; " + r + ": " + sym.name)
      freeRegisters = freeRegisters.tail
      usedRegisters = usedRegisters.updated(sym, r)
      return r
    }  
  
  def free(r: Symbol*) = r.foreach{ s =>
    freeRegisters = usedRegisters(s) :: freeRegisters
    usedRegisters -= s
  }    
  
  def goto(label: String) = R0 := label
  def goto(r: R) = R0 := r
  def gotoIf(c: R)(t: Any) = Instruction("JPC", c, t)
  
  def read:(R => Unit) = (r => Instruction("SYS", 0, r.x))
  def write(r: R) = Instruction("SYS", 1, r.x)
  
  val end = "_end"  
  
  def local (body: => Unit) {    
    val before = (freeRegisters,usedRegisters)
    body
    freeRegisters = before._1
    usedRegisters = before._2    
  }
    
  implicit def deref (f: R => Unit): R = {
    val r = freeRegisters.head
    f(r)
    return r
  }
  
  /** Base trait for assembler instructions */  
  trait Instr {        
    var comment: String = ""
    def || (comment: String) = {
      this.comment = "; " + comment
      this
    }
  }    
  
  /** Assembler instruction 
   * @param name The name of the instruction
   * @param args The arguments to passt to the instruction */
  case class Instruction(name: String, args: Any*) extends Instr {
    instructions.append(this)
    override def toString = {
      name + " " + args.mkString(", ") + comment
    }
  }  
  
  /** Generates a label with the passed identifier */
  case class Label(label: String) extends Instr {
    instructions.append(this)
    override def toString = label + ": " + comment
  }

  /** Represents the number zero for comparisons */
  object O

  /** Base class for registers */
  sealed abstract class R(val x: Int) {    
    /** Assign value n to this register */
    def := (n: Int) = Instruction("MRI", this, n)
    /** Assign value of register Ry to this register */
    def := (Ry: R) = Instruction("MRR", this, Ry)
    /** Assign value of address m to this register */
    def := (m: Address) = Instruction("MRM ", this, m)
    /** Assign label to this register */
    def := (label: String) = Instruction("MRI", this, label)
    /** Assign result of condition c to this register */
    def := (c: Condition) = c match {
      case ISZ(ry) => Instruction("ISZ", this, ry)
      case ISP(ry) => Instruction("ISP", this, ry)
      case ISN(ry) => Instruction("ISN", this, ry)
    }
    def := (f: R => Unit) = f(this)
    /** Assign result of this + Ry to this */        
    def += (Ry: R) = Instruction("ADD", this, Ry)    
    /** Assign result of this - Ry to this */
    def -= (Ry: R) = Instruction("SUB", this, Ry)
    /** Assign result of this * Ry to this */
    def *= (Ry: R) = Instruction("MUL", this, Ry)
    /** Assign result of this / Ry to this */
    def /= (Ry: R) = Instruction("DIV", this, Ry)
    /** Assign result of this mod Ry to this */
    def %= (Ry: R) = Instruction("MOD", this, Ry)
    /** Assign result of binary operation and applied to this and Ry to this */
    def &= (Ry: R) = Instruction("AND", this, Ry)
    /** Assign result of binary operation or applied to this Ry to this */
    def |= (Ry: R) = Instruction("OR", this, Ry)
    /** Assign result of binary operation xor applied to this Ry to this */
    def ^= (Ry: R) = Instruction("XOR", this, Ry)
    /** Create comparison object indicating whether this has value 0 */
    def === (o: O.type) = { ISZ(this) }
    /** Create comparison object indicating whether value of 
     *  this is greater than 0 */
    def > (i: O.type) = { ISP(this) }
    /** Create comparison object indicating whether value of 
     *  this is less than 0 */
    def < (i: O.type) = { ISN(this) }
    
    
    /** Dereference */
    def unary_~ = Address(this)
    
    override def toString = "R" + x
  }  
  
  object R0 extends R(0)
  object R1 extends R(1)
  object R2 extends R(2)
  object R3 extends R(3)
  object R4 extends R(4)
  object R5 extends R(5)
  object R6 extends R(6)
  object R7 extends R(7)

  /** Dereferenced Register */  
  case class Address(address: R) {
    def := (Ry: R): Instr = Instruction("MMR", this, Ry)
    def := (label: String): Unit = local {
      'value := label
      this := 'value
    }
    def := (f: R => Unit): Unit = local {
      'value := f
      this := 'value
    }
    override def toString = "(" + address + ")"
  }

  val stackOverflow = "_stackOverflow"
  
  case class Stack(name: String, size: Int) {            
    val r: R = Symbol(name)
    val start = (name + "_start")
    val end = (name + "_end")
        
    r := start    
    
    def allocate() {
      Label(start)
      Instruction("DAT", size, 0)
      Label(end)
    }
    
    def push(r2: R): Unit = {
      labelCounter += 1
      val skip = "stackPush_" + r2 + "_" + labelCounter
      local { // Check if there is space left
        r -= end                
        gotoIf(r < O)(skip) // if there is space left, we skip the error
        error(stackOverflow)
        Label(skip)
        r += end
      }
      r += one
      ~r := r2     
    }
    
    def pop(): (R => Unit) = (r2 => {
      r2 := ~r
      r -= R1
    })
  
    def top(): (R => Unit) = (r2 => r2 := ~r)
    
    def update(r2: R) {
      ~r := r2
    }
    
    def position = r
  }

  
  implicit def label(s: String): R = {
    val r = freeRegisters.head
    r := s
    return r
  }
  
  implicit def int(i: Int): R = i match {
    case 1 => R1
    case i => 
      val r = freeRegisters.head
      r := i
      return r
    }

  /** Conditions */
  trait Condition
  case class ISZ(Ry: R) extends Condition
  case class ISP(Ry: R) extends Condition
  case class ISN(Ry: R) extends Condition  
  
  implicit def cond(c: Condition): R = {
    val r = freeRegisters.head
    r := c
    return r
  }
}