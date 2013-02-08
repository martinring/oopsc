package de.martinring.oopsc.syntactic

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/**
 * Defines an AST structure to represent an OOPS program
 * @author Martin Ring
 */
trait Element extends Positional

/**
 * Am OOPS program consists of multiple classes.
 */
case class Program(classes: List[Class]) extends Element

/**
 * Visibility annotations provide access protection on class members.
 */
object Visibility extends Enumeration {
  type Visibility = Value 
  /** Private attributes and methods can be only accessed from the class itself */
  val Private = Value
  /**
   * Attributes and methods with visibility 'protected' can only be accessed from 
   * the class itself and classes that derrive from the class which defines the 
   * fields.
   */
  val Protected = Value
  /** Public members are visible everywhere and can be accessed in any context. */
  val Public = Value
}

import Visibility._

trait Declaration extends Element { 
  val name: Name 
  val visibility: Visibility 
}

/**
 * An oops class has a unique name, zero or more attributes and methods (class 
 * members) aswell as a base type. The size of the class is determined during
 * context analysis. 
 */
case class Class(name:       Name,
                 attributes: List[Variable] = Nil,
                 methods:    List[Method]   = Nil,
                 baseType:   Option[Name]   = None,
                 size: Int = Class.headerSize,
                 visited: Boolean = false) extends Declaration {
  val visibility = Public
}

/**
 * Methods are direct children of classes and have a unique name in the class they
 * are contained in. They can have  an optional parameter list and define local 
 * variables. Also they have a return type which defaults to <void>. 
 * The index in the vmt of the containing classes is determined during context analysis.
 */
case class Method(name:       Name,
                  parameters: List[Variable],
                  variables:  List[Variable],
                  body:       List[Statement],
                  typed:      Name = Unknown,
                  index:      Option[Int] = None,
                  visibility: Visibility = Public) extends Declaration

/**
 * Variables can be either declared on class level or as local variables and 
 * parameters in methods. They have a unique name in the current scope. If a 
 * variable is declared as an attribute of a class, the flag isAttribute is set
 * to true. The offset is determined during context analysis.
 */
case class Variable(name:  Name,
                    typed: Name,
                    offset: Option[Int],
                    isAttribute: Boolean,
                    visibility: Visibility = Public) extends Declaration

trait Statement extends Element {
  def returns = false
}

/**
 * A read statement executes a read operation in the OOPSVM and assigns the read
 * value to the operand. The operand is thus required to be a l-value.
 */
case class Read(operand: Expression) extends Statement

/**
 * The write statement evaluates the operand and writes the result to the console.
 */
case class Write(operand: Expression) extends Statement

/**
 * A while statement executes the body in a loop as long as the condition evaluates
 * to true.
 */
case class While(condition: Expression,
                  body:      List[Statement]) extends Statement
 
/**
 * The forever statement is not supported on syntax level but is inserted during
 * optimization if a while statement is found to have a constant condition which
 * allways evaluates to true.
 */
case class Forever(body: List[Statement]) extends Statement

/**
 * An if statement evaluates its contition and then executes the body if the result
 * was true or the elseBody if the result was false. If statements are nested
 * to represent IF ... ELSEIF structures.
 */
case class If(condition: Expression,
              body:      List[Statement],
              elseBody:  List[Statement]) extends Statement {
  override def returns = body.exists(_.returns) && elseBody.exists(_.returns)
}

/**
 * A method call. The called Method must have result type void.
 */
case class Call(call: Expression) extends Statement

/**
 * Evaluates the right expression and assigns the value to the left. The left 
 * expression must be an l-value
 */
case class Assign(left: Expression, right: Expression) extends Statement

/**
 * Return statements are used to return from a method and must have a
 * result if the method has a result type other than void. Otherwise the value
 * must be none.
 */
case class Return(value: Option[Expression] = None, offset: Int = 0) extends Statement {  
  override def returns = true
}
object Return {
  def apply(value: Expression): Return = Return(Some(value))
}

trait Expression extends Element {
  val typed:    Name
  /** Indicates whether this can be assigned to */
  val isLValue: Boolean = false
  /** Creates a Binary Expression adding 'that' to this */
  def + (that: Expression) = Binary("+", this, that, typed)
  /** Creates a Binary Expression subtracting 'that' from this */
  def - (that: Expression) = Binary("-", this, that, typed)
  /** Creates a Binary Expression multipliying this by 'that' */
  def * (that: Expression) = Binary("*", this, that, typed)
  /** Creates a Binary Expression dividing this by 'that' */
  def / (that: Expression) = Binary("/", this, that, typed)
  /** Creates a Binary Expression == */
  def === (that: Expression) = Binary("=", this, that, Class.boolType.name)
  /** Creates a unary expression negating this integer */
  def unary_- = Unary("-", this, typed)
  /** Creates a unary expression negating this boolean */
  def unary_! = Unary("NOT", this, typed)
}

/** A unary expression operates on an operand. It can be either - to negate
  * integers or NOT to negate booleans. */
case class Unary(operator: String,
                 operand:  Expression,
                 typed:    Name = Unknown) extends Expression

/** A Binary expression operates on to operands. It can be one of +,-,*,/,MOD,
  * AND,OR */
case class Binary(operator: String,
                  left:     Expression,
                  right:    Expression,
                  typed:    Name  = Unknown) extends Expression

/** A literal represents a constant value in the code */
case class Literal(value: Int, typed: Name) extends Expression

/** New expressions can be used to create new objects of a class */
case class New(typed: Name) extends Expression

/** Access expressions are used to access members of an object */
case class Access(left:  Expression,
                  right: VarOrCall) extends Expression {
  override val isLValue: Boolean = right.isLValue
  val typed = right.typed
}

/** VarOrCall expressions can be either a variable or a method call. This
  * can not be determined during syntax analysis and is thus done during the
  * context analysis */
case class VarOrCall(name:       Name,
                     parameters: List[Expression] = Nil,
                     typed:      Name = Unknown,
        override val isLValue:   Boolean = false,
                     static:     Boolean = false) extends Expression    

/** A name identifies classes, methods, attributes and variables */
trait Name extends Positional {
  val relative: String
  def label: String = ".error"  
  def / (n: String): AbsoluteName  
  def / (other: Name): AbsoluteName = this / other.relative
}

object Name {
  implicit def string2Name(s: String): Name = RelativeName(s)  
}

/** Relative names are not yet resolved to their full path */
case class RelativeName(relative: String) extends Name {
  override def toString = relative
  def / (n: String) = sys.error("absolute operation on relative name")
}

/** Represents an unknown identifier */
object Unknown extends RelativeName("<unknown>")


// -------------------------------------------------------------------------------------------------------------------
//  Structures for context analysis
// -------------------------------------------------------------------------------------------------------------------

object Root extends Name {
  val relative = "<root>"  
  def / (n: String) = AbsoluteName(List(n))
}

/** An absolute name identifies an object */
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
  val TRUE  = Literal(1, Class.boolType.name)
  val FALSE = Literal(0, Class.boolType.name)
  val NULL  = Literal(0, Class.nullType.name)
  object Int {
    def apply(int: Int): Literal = Literal(int, Class.intType.name)
    def unapply(expr: Expression): Option[Int] = expr match {
      case Literal(x, Class.intType.name) => Some(x)
      case _ => None
    }
  }
}

object Class {
  import Name._
  
  val headerSize = 1
  
  val untyped = Class(Root / "<untyped>" displayAs "untyped")
  
  val intType = Class(Root / "<int>" displayAs "Integer")
  val boolType = Class(Root / "<bool>" displayAs "Boolean")
  val voidType = Class(Root / "<void>" displayAs "void")
  val nullType = Class(Root / "<null>" displayAs "null")
  
  val objectClass = Class(Root / "Object", List(Variable("_cloned", untyped.name, None, true)), Nil)

  val intClass = Class(Root / "Integer",  List(Variable("_value", untyped.name, None, true)), Nil, Some("Object"))
  
  val boolClass = Class(Root / "Boolean", List(Variable("_value", untyped.name, None, true)), Nil, Some("Object"))

  implicit def className(c: Class): Name = c.name
  
  val predefinedClasses = List(objectClass,boolClass,intClass)

  val predefined = predefinedClasses ++ List(intType, boolType, voidType, nullType, untyped)

  val box: Map[Name, Name] = Map(intType.name -> intClass.name,
                                 boolType.name -> boolClass.name)

  val unBox: Map[Name, Name] = Map(intClass.name -> intType.name,
                                   boolClass.name -> boolType.name)
}