package de.martinring.oopsc

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/**
 * Defines an AST structure to represent an OOPS program
 * @author Martin Ring
 */
package object ast {
  trait Element extends Positional

  case class Program(main: Class) extends Element

  trait Declaration extends Element { val name: String }
  case class Class(name: String, members: List[Member] = Nil) extends Declaration {    
    val (attributes, methods) = (members.collect{ case v: Attribute => v }, members.collect{ case m: Method => m })
  }    
  trait Member extends Declaration { val typed: Name }
  case class Attribute(name: String, typed: Name) extends Member
  case class Method(name: String, variables: List[Variable], body: List[Statement]) extends Member { val typed = Name(Class.voidType.name) }
  case class Variable(name: String, typed: Name) extends Declaration
  
  trait Statement extends Element
  case class Read(operand: Expression) extends Statement
  case class Write(operand: Expression) extends Statement
  case class While(condition: Expression, body: List[Statement]) extends Statement
  case class If(condition: Expression, body: List[Statement]) extends Statement
  case class Call(call: Expression) extends Statement
  case class Assign(left: Expression, right: Expression) extends Statement

  trait Expression extends Element { val typed: String }
  case class Unary(operator: String, operand: Expression, typed: String = "?") extends Expression
  case class Binary(operator: String, left: Expression, right: Expression, typed: String  = "?") extends Expression
  case class Literal(value: Int, typed: String) extends Expression
  case class New(typed: String) extends Expression
  case class Access(left: Expression, right: Name, typed: String = "?") extends Expression
  case class Name(name: String, typed: String = "?") extends Expression    

  // -------------------------------------------------------------------------------------------------------------------
  //  Structures for contextual analysis
  // -------------------------------------------------------------------------------------------------------------------
  
  case class Box(expr: Expression, typed: String) extends Expression
  case class UnBox(expr: Expression, typed: String) extends Expression
  case class DeRef(expr: Expression, typed: String) extends Expression

  object Class {
    val intType = Class("<int>")
    val boolType = Class("<bool>")
    val voidType = Class("<void>")
    val nullType = Class("<null>")
    val integer = Class("Integer")

    val predefined = List(integer, intType, boolType, voidType, nullType)
    
    val box: Map[Class, Class] = Map(intType -> integer)
    val unBox: Map[Class, Class] = Map(integer -> intType)
  }

  implicit def stringToName(name: String): Name = Name(name)
  implicit def nameToString(name: Name): String = name.name
}