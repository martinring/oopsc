package de.martinring.oopsc.synthesis

import de.martinring.oopsc._
import scala.collection.mutable.Buffer

/**
 * Integrated DSL for generation of OOPSVM assembler code.
 */
trait Assembler {    
  protected var labelCounter = 0
  
  protected val instructions = Buffer[String]()
  
  protected implicit def instrToString(i: Instr) = i.toString
  
  private var freeRegisters = List[R]()
  private var usedRegisters: Map[Symbol,R] = Map()
  
  protected def init() = {
    instructions.clear
    freeRegisters = List[R](R1,R2,R3,R4,R5,R6,R7)   
    usedRegisters = Map()
    'one := 1
  }    
  
  protected def force(r: R*) = {
    freeRegisters = freeRegisters.filter(!r.contains(_))    
  }
  
  protected def print(r: R) = local {    
    labelCounter = labelCounter + 1
    val l = "_print_" + labelCounter
    val e = "_print_" + labelCounter + "_end"
    label(l)
    'current := ~r
    gotoIf('current === O)(e)
    write('current)
    r += 1
    goto(l)
    label(e)
  }
  
  protected def string(s: String) {
    for (x <- s) Instruction("DAT",1,x.toInt)
    Instruction("DAT",1,0)    
  }  
  
  protected implicit def symToR(sym: Symbol): R =
    usedRegisters.get(sym).getOrElse {
      require(freeRegisters.size > 0, sym.name + " can not be bound " + usedRegisters.mkString(", "))
      val r = freeRegisters.head
      freeRegisters = freeRegisters.tail
      usedRegisters = usedRegisters.updated(sym, r)
      return r
    }              
  
  protected def debugInfo(s: => String) = if (App.arguments.debugMode) {
    instructions.append("; " + s)
  }
  
  protected def free(r: Symbol*) = r.foreach{ s =>
    freeRegisters = usedRegisters(s) :: freeRegisters
    usedRegisters -= s
  }    
  
  protected def goto(label: String) = {
    debugInfo("goto " + label)
    R0 := label
  }
  protected def goto(r: R) = {
    debugInfo("goto " + nameOf(r))
    R0 := r 
  }
  protected def gotoIf(c: R)(t: Any) = {
    debugInfo("if " + nameOf(c) + " then goto " + t)
    Instruction("JPC", c, t)
  }
  
  protected def read:(R => Unit) = (r => Instruction("SYS", 0, r.x))
  protected def write(r: R) = Instruction("SYS", 1, r.x)
  
  protected val end = "_end"  
  
  protected def local (body: => Unit) {    
    val before = (freeRegisters,usedRegisters)
    body
    freeRegisters = before._1
    usedRegisters = before._2    
  }
    
  protected implicit def temporary (f: R => Unit): R = {    
    val r = freeRegisters.head
    freeRegisters = freeRegisters.tail :+ freeRegisters.head
    f(r)
    return r
  }
  
  /** Base trait for assembler instructions */  
  protected trait Instr {        
    var comment: String = ""
    def || (comment: String) = {
      this.comment = "; " + comment
      this
    }
  }    
  
  /** Assembler instruction 
   * @param name The name of the instruction
   * @param args The arguments to passt to the instruction */
  protected case class Instruction(name: String, args: Any*) extends Instr {
    instructions.append(this)
    override def toString = {
      name + " " + args.mkString(", ") + comment
    }
  }  
  
  /** Generates a label with the passed identifier */
  protected def label(label: String) {
    instructions.append(label + ":")    
  }

  /** Represents the number zero for comparisons */
  protected object O

  /** Base class for registers */
  protected sealed abstract class R(val x: Int) {    
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
    def += (Ry: R) = { require(x != 1) ; Instruction("ADD", this, Ry) }
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
    
    def unary_! = this === O
    
    override def toString = "R" + x
  }  
  
  protected object R0 extends R(0)
  protected object R1 extends R(1)
  protected object R2 extends R(2)
  protected object R3 extends R(3)
  protected object R4 extends R(4)
  protected object R5 extends R(5)
  protected object R6 extends R(6)
  protected object R7 extends R(7)

  /** Dereferenced Register */  
  protected case class Address(address: R) {
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
    
  private def nameOf(r: R): String = usedRegisters.toSeq.find(_._2 == r).map(_._1.name).getOrElse(r.toString)
  
  protected case class Stack(name: String, size: Int) {            
    val r: R = Symbol(name)
    val start = (name + "_start")
    val end = (name + "_end")
        
    r := start    
    
    def allocate() {
      label(start)
      Instruction("DAT", size, 0)
      label(end)
    }
    
    def push(label: String): Unit = local {
      debugInfo(name + ".push(" + label + ")") 
      'val := label
      r += 1
      ~r := 'val
    }
    
    def push(r2: R): Unit = {
      debugInfo(name + ".push(" + nameOf(r2) + ")")      
      r += 1
      ~r := r2
    }
    
    def pop(): (R => Unit) = (r2 => {
      debugInfo(nameOf(r2)+" = "+name+".pop")  
      r2 := ~r
      r -= 1
    })
  
    def top(): (R => Unit) = { r2 => 
      debugInfo(nameOf(r2)+" = "+name+".top")
      r2 := ~r
    }
    
    def update(r2: R) {
      debugInfo(name + ".update(" + nameOf(r2) + ")")
      ~r := r2
    }
    
    def position = r
  }

  
  protected implicit def labelValue(s: String): R = {
    val r = freeRegisters.head
    r := s
    return r
  }
  
  protected implicit def int(i: Int): R = i match {
    case 1 => 'one
    case i => 
      val r = freeRegisters.head
      r := i
      return r
    }

  /** Conditions */
  protected trait Condition
  protected case class ISZ(Ry: R) extends Condition
  protected case class ISP(Ry: R) extends Condition
  protected case class ISN(Ry: R) extends Condition  
  
  protected implicit def cond(c: Condition): R = {
    val r = freeRegisters.head
    r := c
    return r
  }
}