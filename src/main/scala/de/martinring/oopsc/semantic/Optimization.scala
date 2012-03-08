package de.martinring.oopsc.semantic

import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.semantic.Transform._

/** 
 * For optimization of OOPS code
 * @author Martin Ring
 */
object Optimization {
  import Literal._
    
  /**
   * Optimizes an expression by calculating expressions including literals at
   * compile time.
   */
  def optimize(e: Expression): Expression = e match {                   
    case Unary(op,e,t) => optimizeUnary(Unary(op,optimize(e),t))
    case Binary(op,l,r,t) => optimizeBinary(Binary(op,optimize(l),optimize(r),t))
    case Box(e,t) => optimize(e) match {
      case UnBox(e,t) => e
      case e => Box(e,t)
    }    
    case expr => expr
  }
  
  private def optimizeUnary(e: Unary): Expression = e match {
    case Minus(Minus(x)) => x
    case Minus(Int(x)) => Int(-x)
    case Not(Not(x)) => x
    case Not(TRUE) => FALSE
    case Not(FALSE) => TRUE
    case x => x
  }
   
  private def optimizeBinary(e: Binary): Expression = e.operator match {
    case "+" | "-" => e match {
      case Int(x) + Int(y) => Int(x+y)
      case Int(x) - Int(y) => Int(x-y)
      case Int(0) + x => x
      case x + Int(0) => x
      case x + Minus(y) => x - y
      case Int(0) - x => Minus(x)
      case x - Int(0) => x
      case x - Minus(y) => x + y
      case x => x
    }
    case "*" => e match {
      case Int(x) * Int(y) => Int(x*y)
      case Int(0) * _ => Int(0)
      case _ * Int(0) => Int(0)
      case Int(1) * x => x
      case x * Int(1) => x
      case Int(c) * Minus(x) => Int(-c) * x
      case Minus(x) * Int(c) => Int(-c) * x      
      case Minus(x) * Minus(y) => x * y
      case Minus(x) * y => Minus(x * y)
      case x * Minus(y) => Minus(x * y)      
      case x => x
    }
    case "/" => e match {
      case Int(x) / Int(y) => Int(x/y)
      case Int(0) / x => Int(0)
      case x / Int(1) => x
      case Minus(x) / Minus(y) => x / y
      case Minus(x) / Int(c) => x / Int(-c)
      case Minus(x) / y => Minus(x / y)
      case x / Minus(y) => Minus(x / y)
      case x => x      
    }
    case "AND" | "OR" => e match {
      case FALSE AND x => FALSE
      case x AND FALSE => FALSE
      case TRUE AND x => x
      case x AND TRUE => x
      case FALSE OR x => x
      case x OR FALSE => x
      case TRUE OR x => TRUE
      case x OR TRUE => TRUE
      case x => x
    }
    case _ => e
  }
  
  /**
   * optimizes all classes of a program
   */
  def optimize(p: Program): Transform[Program] = for {
    classes <- sequence(p.classes map optimize)
  } yield Program(classes) at p

  /**
   * optimizes all methods of a class and updates the binding
   */
  def optimize(c: Class): Transform[Class] = for {
    methods    <- sequence(c.methods map optimize)
    result     <- update(c.copy(methods = methods) at c)
  } yield result

  /**
   * optimizes all statements of a method and updates the binding
   */
  def optimize(m: Method): Transform[Method] = for {
    result     <- update(m.copy(body = (m.body map optimize) flatten) at m)
  } yield result

  /**
   * optimizes a statement by optimizing all contained expressions and statements
   * and:
   * 
   *  - `WHILE FALSE DO ...` gets erased
   *  - `WHILE TRUE DO x` becomes `FOREVER DO x`
   *  - `IF TRUE THEN x ELSE y` becomes `x`
   *  - `IF FALSE THEN x ELSE y` becomes `y`
   */
  def optimize(st: Statement): List[Statement] = st match {
    case w: Write =>
      List(Write(optimize(w.operand)) at w)

    case w: While =>
      lazy val body = (w.body map optimize).flatten
      optimize(w.condition) match {
        case FALSE => Nil // while false => _
        case TRUE  => List(Forever(body))
        case cond => List(While(cond, body) at w)
      }

    case i: If =>
      lazy val body = (i.body map optimize).flatten
      lazy val elseBody = (i.elseBody map optimize).flatten
      optimize(i.condition) match {
        case TRUE  => body // if true then else => then
        case FALSE => elseBody // if false then else => else
        case cond => List(If(cond, body, elseBody) at i)
    }
          
    case c: Call =>
      List(Call(optimize(c.call)) at c)

    case a: Assign =>
      List(Assign(optimize(a.left), optimize(a.right)) at a)

    case r@Return(Some(expr),o) =>
      List(Return(Some(optimize(expr)),o) at r)

    case statement => List(statement)
  }     

  /** Extractor for unary expressions. Optimizes the operand. */
  private abstract class UnaryExtractor(op: String) {
    def apply(expr: Expression): Expression = Unary(op, expr, expr.typed)
    def unapply(expr: Expression): Option[Expression] = expr match {
      case Unary(`op`, x, _) => Some(optimize(x))
      case _ => None
    }
  }    
  
  /** Extractor for binary expressions. Optimizes the operand. */
  private abstract class BinaryExtractor(op: String) {
    def unapply(expr: Expression): Option[(Expression,Expression)] = expr match {
      case Binary(`op`,x,y,_) => Some(optimize(x),optimize(y))
      case _ => None
    }
  }
  
  private object Minus extends UnaryExtractor("-")
  private object Not extends UnaryExtractor("NOT")  
  
  private object - extends BinaryExtractor("-")  
  private object + extends BinaryExtractor("+")
  private object * extends BinaryExtractor("*")
  private object / extends BinaryExtractor("/")
  private object AND extends BinaryExtractor("AND")  
  private object OR extends BinaryExtractor("OR")
}
