package de.martinring.oopsc

object Assembler {
  var code: String = "; OOPS Assembler Code Output ;"

  private def instruction(name: String, a: Any, b: Any) = code += "\n%s %s, %s".format(name, a.toString, b.toString)
  implicit def comment(u: Unit) = new {
    def \\ (s: String) = code += "; " + s
  }

  object O

  object R {
    def update(x: Int, n: Int) = instruction("MRI", R(x), n)
    def update(x: Int, Ry: R) = instruction("MRR", R(x), Ry)
    def update(x: Int, m: Address) = instruction("MRM ", this, m)
    def update(x: Int, c: Condition) = c match {
      case ISZ(ry) => instruction("ISZ", R(x), ry)
      case ISP(ry) => instruction("ISP", R(x), ry)
      case ISN(ry) => instruction("ISN", R(x), ry)
    }
  }

  case class R(x: Int) {
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