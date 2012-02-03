package de.martinring.oopsc

object Assembler {
  def label(s: String) = Instruction(s + ":")

  trait Instr {
    var comment: String = ""
    def || (comment: String) = {
      this.comment = "; " + comment
      this
    }
  }

  case class Instruction(name: String, args: Any*) extends Instr {
    override def toString = name + " " + args.mkString(", ") + comment
  }

  case object No extends Instr {
    override def toString = comment
  }

  case class Label(label: String) extends Instr {
    override def toString = label + ": " + comment
  }

  case class Instructions(title: String)(val is: Instr*) extends Instr {
    override def toString = "; " + title + "\n" + is.mkString("\n").indent(2) + "\n" + "; END " + title
  }

  object O

  sealed abstract class R(x: Int) {
    def << (n: Int) = Instruction("MRI", this, n)
    def << (Ry: R) = Instruction("MRR", this, Ry)
    def << (m: Address) = Instruction("MRM ", this, m)
    def << (label: String) = Instruction("MRI", this, label)
    def << (c: Condition) = c match {
      case ISZ(ry) => Instruction("ISZ", this, ry)
      case ISP(ry) => Instruction("ISP", this, ry)
      case ISN(ry) => Instruction("ISN", this, ry)
    }
    def <<+ (Ry: R) = Instruction("ADD", this, Ry)
    def <<- (Ry: R) = Instruction("SUB", this, Ry)
    def <<* (Ry: R) = Instruction("MUL", this, Ry)
    def <</ (Ry: R) = Instruction("DIV", this, Ry)
    def <<% (Ry: R) = Instruction("MOD", this, Ry)
    def <<& (Ry: R) = Instruction("AND", this, Ry)
    def <<| (Ry: R) = Instruction("OR", this, Ry)
    def <<^ (Ry: R) = Instruction("XOR", this, Ry)
    def === (o: O.type) = { ISZ(this) }
    def > (i: O.type) = { ISP(this) }
    def < (i: O.type) = { ISN(this) }

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

  case class Address(address: R) {
    def << (Ry: R) = Instruction("MMR", this, Ry)    
    override def toString = "(" + address + ")"
  }

  trait Condition
  case class ISZ(Ry: R) extends Condition
  case class ISP(Ry: R) extends Condition
  case class ISN(Ry: R) extends Condition
}