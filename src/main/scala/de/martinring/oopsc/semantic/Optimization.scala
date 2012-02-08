/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.martinring.oopsc

import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.Transform._

object Optimization {
  import Literal._
    
  def optimize(e: Expression): Expression = e match {                   
    case Unary(op,e,t) => optimizeUnary(Unary(op,optimize(e),t))
    case Binary(op,l,r,t) => optimizeBinary(Binary(op,optimize(l),optimize(r),t))
    case Box(e,t) => optimize(e) match {
      case UnBox(e,t) => e
      case e => Box(e,t)
    }    
    case expr => expr
  }
  
  def optimizeUnary(e: Unary): Expression = e match {
    case Minus(Minus(x)) => x
    case Not(Not(x)) => x
    case x => x
  }
   
  def optimizeBinary(e: Binary): Expression = e.operator match {
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
      case False ∧ x => False
      case x ∧ False => False
      case True ∧ x => x
      case x ∧ True => x
      case False ∨ x => x
      case x ∨ False => x
      case True ∨ x => True
      case x ∨ True => True
      case x => x
    }
    case _ => e
  }
  
  
  def optimize(p: Program): Transform[Program] = for {
    classes <- sequence(p.classes map optimize)
  } yield Program(classes) at p

  def optimize(c: Class): Transform[Class] = for {
    methods    <- sequence(c.methods map optimize)
    result     <- update(c.copy(methods = methods) at c)
  } yield result

  def optimize(m: Method): Transform[Method] = for {
    result     <- update(m.copy(body = (m.body map optimize) flatten) at m)
  } yield result

  def optimize(st: Statement): List[Statement] = st match {
    case w: Write =>
      List(Write(optimize(w.operand)))

    case w: While =>
      lazy val body = (w.body map optimize).flatten
      optimize(w.condition) match {
        case Literal.False => Nil // while false => _
        case Literal.True  => List(Forever(body))
        case cond => List(While(cond, body))
      }

    case i: If =>
      lazy val body = (i.body map optimize).flatten
      lazy val elseBody = (i.elseBody map optimize).flatten
      optimize(i.condition) match {
        case Literal.True  => body // if true then else => then
        case Literal.False => elseBody // if false then else => else
        case cond => List(If(cond, body, elseBody))
    }
          
    case c: Call =>
      List(Call(optimize(c.call)))

    case a: Assign =>
      List(Assign(optimize(a.left), optimize(a.right)))

    case r@Return(Some(expr)) =>
      List(Return(Some(optimize(expr))))

    case statement => List(statement)
  }     

  
  object Minus {
    def apply(expr: Expression): Expression = Unary("-", expr, expr.typed)
    def unapply(expr: Expression): Option[Expression] = expr match {
      case Unary("-", x, _) => Some(x)
      case _ => None
    }
  }    
  
  object Not {
    def apply(expr: Expression): Expression = Unary("-", expr, expr.typed)
    def unapply(expr: Expression): Option[Expression] = expr match {
      case Unary("-", x, _) => Some(x)
      case _ => None
    }
  }      
  
  object - {    
    def unapply(expr: Expression): Option[(Expression,Expression)] = expr match {      
      case Binary("-", x, y, _ ) => Some(optimize(x),optimize(y))
      case _ => None
    }
  }
  
  object + {    
    def unapply(expr: Expression): Option[(Expression,Expression)] = expr match {
      case Binary("+", x, y, _ ) => Some(x,y)
      case _ => None
    }
  }
  
  object * {    
    def unapply(expr: Expression): Option[(Expression,Expression)] = expr match {
      case Binary("*", x, y, _ ) => Some(x,y)
      case _ => None
    }
  }  
  
  object / {    
    def unapply(expr: Expression): Option[(Expression,Expression)] = expr match {
      case Binary("/", x, y, _ ) => Some(x,y)
      case _ => None
    }
  }  
  
  object ∧ {    
    def unapply(expr: Expression): Option[(Expression,Expression)] = expr match {
      case Binary("AND", x, y, _ ) => Some(x,y)
      case _ => None
    }
  }  
  
  object ∨ {    
    def unapply(expr: Expression): Option[(Expression,Expression)] = expr match {
      case Binary("OR", x, y, _ ) => Some(x,y)
      case _ => None
    }
  }  
}
