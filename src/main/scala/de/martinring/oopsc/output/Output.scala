package de.martinring.oopsc.output

import scala.io.Source
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.lexical.Scanner._
//import de.martinring.oopsc.synthesis.Code
import util.parsing.input.Positional
import com.sun.org.apache.bcel.internal.classfile.LocalVariable

/**
 * The [[Output]] object provides functions for formatting internal data structures 
 * to be human readable.
 * @author Martin Ring
 */
object Output {
  /**
   * Print out tokens that [[scanner]] recognized in a human readable format.
   */
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

  /**
   * Print abstract syntax tree in a human readable format.
   */
  def apply(e: Element) {
    println(" Line | Col. | Parsed Element ")
    println("------+------+-----------------------------------------------------------------")
    println(element(e))
    println
  }

  private val lf = "\n"

  private def indent(s: Any*): String =
    s.map {
      case e: Element => lf + element(e).split(lf).map{x => val (l,r) = x.splitAt(noPos.length); l + ". " + r}.mkString(lf)
      case s => lf + s.toString.split(lf).map{x => val (l,r) = x.splitAt(noPos.length); l + ". " + r}.mkString(lf)
    }.mkString

  private def pos(element: Positional): String = (element.pos.line, element.pos.column) match {
    case (0,0) => noPos
    case (l,c) => " %-5s| %-5s| ".format(l,c)
  }

  private val noPos = "      |      | "

  private def element(el: Element): String = el match {
    case Program(classes) => noPos + "PROGRAM" + indent(classes :_*)

    case c: Class =>
      pos(c) + "CLASS " + id(c.name) + " EXTENDS " + c.baseType.map(_.toString).getOrElse("*") + indent(
                                          noPos + "ATTRIBUTES" + indent(c.attributes :_*),
                                          noPos + "METHODS" + indent (c.methods :_*))

    case m@Method(name, params, vars, body, typed, index, vis) =>
      pos(m) + vis.toString.toUpperCase + " METHOD " + id(name) + ": " + id(typed) + (index.map(" ("+_+")").getOrElse("")) + indent(
                                          noPos + "PARAMETERS" + indent(params :_*),
                                          noPos + "VARIABLES" + indent(vars :_*),
                                          noPos + "BODY" + indent(body :_*))

    case v@Variable(name, t, offset, attr, vis) =>
      pos(v) + vis.toString.toUpperCase + " " + id(name) + ": " + id(t) + offset.map(" (" +_+ ")").getOrElse("")

    case r@Read(operand) =>
      pos(r) + "READ" + indent(operand)

    case w@Write(operand) =>
      pos(w) + "WRITE" + indent(operand)

    case w@While(condition, body) =>
      pos(w) + "WHILE" + indent(condition, noPos + "DO" + indent(body :_*))

    case w@Forever(body) =>
      pos(w) + "FOREVER" + indent(body :_*)
      
    case i@If(condition, body, Nil) =>
      pos(i) + "IF" + indent(condition, noPos + "DO" + indent(body :_*))

    case i@If(condition, body, elseBody) =>
      pos(i) + "IF" + indent(condition, noPos + "DO" + indent(body :_*), noPos + "ELSE" + indent(elseBody : _*))

    case c@Call(call) =>
      pos(c) + "CALL" + indent(call)

    case a@Assign(left, right) =>
      pos(a) + "ASSIGN" + indent(left, right)

    case r@Return(expr) =>
      pos(r) + "RETURN" + (expr match {
        case Some(e) => indent(e)
        case _       => ""
      })


    case u@Unary(op, operand, t) =>
      pos(u) + op + typed(t) + indent(operand)

    case b@Binary(op, left, right, t) =>
      pos(b) + op + typed(t) + indent(left, right)

    case l@Literal(value, t) =>
      pos(l) + value + typed(t)

    case n@New(newType) =>
      pos(n) + "NEW " + newType

    case a@Access(left, right) =>
      pos(a) + "ACCESS" + typed(a.typed, a.isLValue) + indent(left, right)

    case n: VarOrCall =>
      pos(n) + id(n.name) + typed(n.typed, n.isLValue) + (if (n.parameters.isEmpty) ""
      else indent(noPos + "PARAMETERS" + indent(n.parameters :_*)))

    case b@Box(expr: Expression, t) =>
      pos(b) + "BOX" + typed(t, false) + indent(expr)

    case u@UnBox(expr: Expression, t) =>
      pos(u) + "UNBOX" + typed(t) + indent(expr)

    case d@DeRef(expr: Expression, t) =>
      pos(d) + "DEREF" + typed(t) + indent(expr)
  }

  private def varorcall(n: VarOrCall) = id(n.name) + "PARAMS" + indent(n.parameters)

  private def id(identifier: Name) = identifier.toString

  private def typed(t: Name, lv: Boolean = false) = t.relative match {
    case "?"  => ""
    case t => ": " + t + (if(lv)"*"else"")
  }
}