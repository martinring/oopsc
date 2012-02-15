package de.martinring.oopsc.synthesis

import de.martinring.oopsc._

/**
 * Integrated DSL for generation of OOPSVM assembler code.
 */
object Assembler {
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
    override def toString = name + " " + args.mkString(", ") + comment
  }
  
  /** Empty Instruction */
  case object No extends Instr {
    override def toString = comment
  }

  /** Generates a label with the passed identifier */
  case class Label(label: String) extends Instr {
    override def toString = label + ": " + comment
  }

  /** Compile zero or more Instructions to a single Instruction */
  case class Instructions(title: String)(val is: Instr*) extends Instr {
    override def toString = "; " + title + "\n" + is.mkString("\n").indent(2) + "\n" + "; END " + title
  }

  /** Represents the number zero for comparisons */
  object O

  /** Base class for registers */
  sealed abstract class R(x: Int) {
    /** Assign value n to this register */
    def << (n: Int) = Instruction("MRI", this, n)
    /** Assign value of register Ry to this register */
    def << (Ry: R) = Instruction("MRR", this, Ry)
    /** Assign value of address m to this register */
    def << (m: Address) = Instruction("MRM ", this, m)
    /** Assign label to this register */
    def << (label: String) = Instruction("MRI", this, label)
    /** Assign result of condition c to this register */
    def << (c: Condition) = c match {
      case ISZ(ry) => Instruction("ISZ", this, ry)
      case ISP(ry) => Instruction("ISP", this, ry)
      case ISN(ry) => Instruction("ISN", this, ry)
    }
    /** Assign result of this + Ry to this */
    def <<+ (Ry: R) = Instruction("ADD", this, Ry)
    /** Assign result of this - Ry to this */
    def <<- (Ry: R) = Instruction("SUB", this, Ry)
    /** Assign result of this * Ry to this */
    def <<* (Ry: R) = Instruction("MUL", this, Ry)
    /** Assign result of this / Ry to this */
    def <</ (Ry: R) = Instruction("DIV", this, Ry)
    /** Assign result of this mod Ry to this */
    def <<% (Ry: R) = Instruction("MOD", this, Ry)
    /** Assign result of binary operation and applied to this and Ry to this */
    def <<& (Ry: R) = Instruction("AND", this, Ry)
    /** Assign result of binary operation or applied to this Ry to this */
    def <<| (Ry: R) = Instruction("OR", this, Ry)
    /** Assign result of binary operation xor applied to this Ry to this */
    def <<^ (Ry: R) = Instruction("XOR", this, Ry)
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
    def << (Ry: R) = Instruction("MMR", this, Ry)    
    override def toString = "(" + address + ")"
  }

  /** Conditions */
  trait Condition
  case class ISZ(Ry: R) extends Condition
  case class ISP(Ry: R) extends Condition
  case class ISN(Ry: R) extends Condition
}