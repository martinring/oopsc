package de.martinring.oopsc

import scala.io.Source
import de.martinring.oopsc.ast._
import de.martinring.oopsc.parsing.Lexical._
import util.parsing.input.Positional
import com.sun.org.apache.bcel.internal.classfile.LocalVariable

/*
 * @ author Martin Ring
 */
object Output {
  def apply(scanner: Scanner) {
    def p(scanner: Scanner) {
      if (scanner.first != EOF) {
        printf(" %-5s| %-5s| %s\n", scanner.pos.line, scanner.pos.column, scanner.first)
        p(scanner.rest)
      }
    }
    println(" Line | Col. | Parsed Token ")
    println("------+------+-----------------------------------------------------------------")
    p(scanner)
    println
  }

  def apply(e: Element) {
    println(" Line | Col. | Parsed Element ")
    println("------+------+-----------------------------------------------------------------")
    println(element(e))
    println
  }

  val lf = "\n"

  def indent(s: Any*): String =
    s.map {
      case e: Element => lf + element(e).split(lf).map{x => val (l,r) = x.splitAt(noPos.length); l + ". " + r}.mkString(lf)
      case s => lf + s.toString.split(lf).map{x => val (l,r) = x.splitAt(noPos.length); l + ". " + r}.mkString(lf)
    }.mkString

  def pos(element: Positional): String = (element.pos.line, element.pos.column) match {
    case (0,0) => noPos
    case (l,c) => " %-5s| %-5s| ".format(l,c)
  }

  val noPos = "      |      | "

  def element(el: Element): String = el match {
    case Program(main) => noPos + "PROGRAM" + indent(main)
      
    case c: Class =>
      pos(c) + "CLASS " + c.name + indent(noPos + "ATTRIBUTES" + indent(c.attributes :_*), 
                                          noPos + "METHODS" + indent (c.methods :_*))
    case m@Method(name, vars, body) =>
      pos(m) + "METHOD " + name + indent(noPos + "VARIABLES" + indent(vars :_*),
                                         noPos + "BODY" + indent(body :_*))
    case v@Variable(name, t) => pos(v) + name + ": " + this.name(t)
    case a@Attribute(name, t) => pos(a) + name + ": " + this.name(t)
      
    case r@Read(operand) => pos(r) + "READ" + indent(operand)
    case w@Write(operand) => pos(w) + "WRITE" + indent(operand)
    case w@While(condition, body) => pos(w) + "WHILE" + indent(condition, noPos + "DO" + indent(body :_*))      
    case i@If(condition, body) => pos(i) + "IF" + indent(condition, noPos + "DO" + indent(body :_*))
    case c@Call(call) => pos(c) + "CALL" + indent(call)
    case a@Assign(left, right) => pos(a) + "ASSIGN" + indent(left, right)
      
    case u@Unary(op, operand, t) => pos(u) + delimiters(op) + typed(t) + indent(operand)
    case b@Binary(op, left, right, t) => pos(b) + delimiters(op) + typed(t) + indent(left, right)
    case l@Literal(value, t) => pos(l) + value + typed(t)
    case n@New(newType) => pos(n) + "NEW " + name(newType)
    case a@Access(left: Expression, right: Name, t) => pos(a) + "ACCESS" + typed(t) + indent(left, right)
    case n: Name => pos(n) + name(n) + typed(n.typed)
    case b@Box(expr: Expression, t) => pos(b) + "BOX" + typed(t) + indent(expr)
    case u@UnBox(expr: Expression, t) => pos(u) + "UNBOX" + typed(t) + indent(expr)
    case d@DeRef(expr: Expression, t) => pos(d) + "DEREF" + typed(t) + indent(expr)
  }

  def name(n: Name) = n.name

  def typed(t: Name) = t match {    
    case Name("?",_) => ""
    case _ => ": " + t.name
  }
}