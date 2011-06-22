package de.martinring.oopsc

object Assembler {
  private def instruction(name: String, a: Any, b: Any): String = "%s %s, %s".format(name, a.toString, b.toString)
   
  object O

  case class R(x: Int) {
    def := (n: Int) = instruction("MRI", this, n)
    def := (Ry: R) = instruction("MRR", this, Ry)
    def := (m: Address) = instruction("MRM ", this, m)
    def := (label: String) = instruction("MRI", this, label)
    def := (c: Condition) = c match {
      case ISZ(ry) => instruction("ISZ", this, ry)
      case ISP(ry) => instruction("ISP", this, ry)
      case ISN(ry) => instruction("ISN", this, ry)
    }
    def += (Ry: R) = instruction("ADD", this, Ry)
    def -= (Ry: R) = instruction("SUB", this, Ry)
    def *= (Ry: R) = instruction("MUL", this, Ry)
    def /= (Ry: R) = instruction("DIV", this, Ry)
    def %= (Ry: R) = instruction("MOD", this, Ry)
    def &= (Ry: R) = instruction("AND", this, Ry)
    def |= (Ry: R) = instruction("OR", this, Ry)
    def ^= (Ry: R) = instruction("XOR", this, Ry)
    def === (o: O.type) = { ISZ(this) }
    def > (i: O.type) = { ISP(this) }
    def < (i: O.type) = { ISN(this) }

    override def toString = "R" + x
  }

  object Address { def update(Rx: R, Ry: R) = instruction("MMR", Address(Rx), Ry) }
  case class Address(address: R) { override def toString = "(" + address + ")" }

  trait Condition
  case class ISZ(Ry: R) extends Condition
  case class ISP(Ry: R) extends Condition
  case class ISN(Ry: R) extends Condition
}