package de.martinring.oopsc.syntactic

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/**
 * Defines an AST structure to represent an OOPS program
 * @author Martin Ring
 */

trait Element extends Positional

case class Program(classes: List[Class]) extends Element

object Visibility extends Enumeration {
  type Visibility = Value
  val Private, Protected, Public = Value
}

import Visibility._

trait Declaration extends Element { 
  val name: Name 
  val visibility: Visibility 
}

case class Class(name:       Name,
                 attributes: List[Variable] = Nil,
                 methods:    List[Method]   = Nil,
                 baseType:   Option[Name]   = None,
                 size: Int = Class.headerSize,
                 visited: Boolean = false) extends Declaration {
  val visibility = Public
}

case class Method(name:       Name,
                  parameters: List[Variable],
                  variables:  List[Variable],
                  body:       List[Statement],
                  typed:      Name = Unknown,
                  index:      Option[Int] = None,
                  visibility: Visibility = Public) extends Declaration

case class Variable(name:  Name,
                    typed: Name,
                    offset: Option[Int],
                    isAttribute: Boolean,
                    visibility: Visibility = Public) extends Declaration

trait Statement extends Element {
  def returns = false
}

case class Read(operand: Expression) extends Statement

case class Write(operand: Expression) extends Statement

case class While(condition: Expression,
                  body:      List[Statement]) extends Statement
 
case class Forever(body: List[Statement]) extends Statement

case class If(condition: Expression,
              body:      List[Statement],
              elseBody:  List[Statement]) extends Statement {
  override def returns = body.exists(_.returns) && elseBody.exists(_.returns)
}

case class Call(call: Expression) extends Statement

case class Assign(left: Expression, right: Expression) extends Statement

case class Return(value: Option[Expression] = None) extends Statement {
  override def returns = true
}

trait Expression extends Element {
  val typed:    Name
  val isLValue: Boolean = false
  def + (that: Expression) = Binary("+", this, that, typed)
  def - (that: Expression) = Binary("-", this, that, typed)
  def * (that: Expression) = Binary("*", this, that, typed)
  def / (that: Expression) = Binary("/", this, that, typed)
  def unary_- = Unary("-", this, typed)
  def unary_! = Unary("NOT", this, typed)
}

case class Unary(operator: String,
                 operand:  Expression,
                 typed:    Name = Unknown) extends Expression

case class Binary(operator: String,
                  left:     Expression,
                  right:    Expression,
                  typed:    Name  = Unknown) extends Expression

case class Literal(value: Int, typed: Name) extends Expression

case class New(typed: Name) extends Expression

case class Access(left:  Expression,
                  right: VarOrCall) extends Expression {
  override val isLValue: Boolean = right.isLValue
  val typed = right.typed
}

case class VarOrCall(name:       Name,
                     parameters: List[Expression] = Nil,
                     typed:      Name = Unknown,
        override val isLValue:   Boolean = false,
                     static:     Boolean = false) extends Expression    

trait Name extends Positional {
  val relative: String
  def label: String = ".error"
  implicit def string2Absolute(s: String) = AbsoluteName(List(s))
}

case class RelativeName(relative: String) extends Name {
  override def toString = relative
}

object Unknown extends RelativeName("<unknown>")


// -------------------------------------------------------------------------------------------------------------------
//  Structures for context analysis
// -------------------------------------------------------------------------------------------------------------------

object Root {
  def / (n: String) = AbsoluteName(List(n))
}

case class AbsoluteName(path:      List[String],
                        displayAs: Option[String] = None) extends Name {

  val relative = path.last

  override def toString = displayAs match {
    case None    => path.mkString(".")
    case Some(s) => s
  }

  override def label = path.mkString("_")

  override def equals(other: Any) = other match {
    case r: AbsoluteName => path == r.path
    case _ => false
  }

  def displayAs(s: String) = AbsoluteName(path, Some(s))

  def / (other: String) = AbsoluteName(path :+ other)
  def / (other: Name)   = AbsoluteName(path :+ other.relative)
}


case class Box(expr:  Expression,
                typed: Name) extends Expression {
  override val isLValue = false
}

case class UnBox(expr:  Expression,
                  typed: Name) extends Expression

case class DeRef(expr:  Expression,
                  typed: Name) extends Expression

object Literal {
  val True  = Literal(1, Class.boolType.name)
  val False = Literal(0, Class.boolType.name)
  val Null  = Literal(0, Class.nullType.name)
  object Int {
    def apply(int: Int): Literal = Literal(int, Class.intType.name)
    def unapply(expr: Expression): Option[Int] = expr match {
      case Literal(x, Class.intType.name) => Some(x)
      case _ => None
    }
  }
}

object Class {
  val headerSize = 1

  val intType = Class(Root / "<int>" displayAs "Integer")
  val boolType = Class(Root / "<bool>" displayAs "Boolean")
  val voidType = Class(Root / "<void>" displayAs "Void")
  val nullType = Class(Root / "<null>" displayAs "Null")
  val objectClass = Class(Root / "Object")
  val intClass = new Class(Root / "Integer", Nil, Nil, Some(Root / "Object")) {
    override val size = 1 + headerSize
  }
  val boolClass = new Class(Root / "Boolean", Nil, Nil, Some(Root / "Object")) {
    override val size = 1 + headerSize
  }

  val predefinedClasses = List(boolClass,intClass,objectClass)

  val predefined = List(boolClass, intClass, objectClass, intType, boolType, voidType, nullType)

  val box: Map[Class, Class] = Map(intType -> intClass,
                                    boolType -> boolClass)

  val unBox: Map[Class, Class] = Map(intClass -> intType,
                                      boolClass -> boolType)
}
