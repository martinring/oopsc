/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.martinring.oopsc

import de.martinring.oopsc.ast._
import de.martinring.oopsc.ast.Class._
import de.martinring.oopsc.Transform._
import java.lang.Boolean

case class Context(declarations: Declarations, currentType: String = "", currentMethod: String = "", counter: Int = 0)

/*
 * Object for the contextual analysis. Utilizes the @see Transform monad.
 */
object ContextAnalysis {  
  def program(p: Program): Transform[Program] = for {
    _    <- bind(predefined)
    _    <- bind(p.main)
    _    <- require(p.main.name == "Main")                   (Error(p.main.pos, "No main class exists"))
    _    <- require(p.main.methods.exists(_.name == "main")) (Error(p.main.pos, "Main class doesn't contain a main method"))
    main <- clazz(p.main)
    _    <- rebind(main)
  } yield Program(main) at p
  
  def clazz(c: Class): Transform[Class] = for {
    _          <- enter(c, c.attributes ++ c.methods)
    attributes <- sequence(c.attributes map attribute )
    o          <- incOffset(0)    
    methods    <- sequence(c.methods map method(c))
    _          <- rebind(methods)
    _          <- leave
  } yield Class(c.name, attributes ++ methods, Some(o)) at c
  
  def attribute(a: Attribute): Transform[Attribute] = for {
    o <- incOffset(1)
    _ <- resolveClass(a.typed)
  } yield a.copy(offset = o)
  
  def variable(v: Variable): Transform[Variable] = for {
    o <- incOffset(1)
    _ <- resolveClass(v.typed)        
  } yield v.copy(offset = o)
  
  def method(self: Class)(m: Method): Transform[Method] = for {
    _         <- enter(m, m.variables)    
    _         <- bind(Variable("SELF", self.name, -2))
    _         <- incOffset(1) // skip return address
    variables <- sequence(m.variables map variable)
    _         <- rebind(variables)
    body      <- sequence(m.body map statement)
    _         <- leave
  } yield Method(m.name, variables, body) at m
    
  def statement(st: Statement): Transform[Statement] = st match {
    case r: Read => for {
      operand <- expression(r.operand)
    } yield Read(operand) at r
    
    case w: Write => for {
      operand <- expression(w.operand) >>= unBox >>= requireType(intType)
    } yield Write(operand) at w
      
    case w: While => for {
      condition <- expression(w.condition) >>= unBox >>= requireType(boolType)
      body      <- sequence(w.body map statement)
    } yield While(condition, body) at w
      
    case i: If => for {
      condition <- expression(i.condition) >>= unBox >>= requireType(boolType)
      body      <- sequence(i.body map statement)
    } yield If(condition, body) at i
    
    case c: Call => for {
      call <- expression(c.call)      
    } yield Call(call) at c
    
    case a: Assign => for {
      left  <- expression(a.left)
      _     <- require(left.lvalue) (Error(left.pos, "l-value expected"))
      t     <- resolveClass(left.typed)
      right <- expression(a.right) >>= box >>= requireType(t)
    } yield Assign(left, right) at a      
  }  
  
  def expression(e: Expression): Transform[Expression] = e match {
    case u: Unary => u.operator match {
      case "-" => for {
        operand  <- expression(u.operand) >>= unBox >>= requireType(intType)
      } yield new Unary(u.operator, operand, intType.name) at u
    }
      
    case b: Binary => b.operator match {
      case "+" | "-" | "*" | "/" | "MOD" => for {
        left  <- expression(b.left) >>= unBox >>= requireType(intType)
        right <- expression(b.right) >>= unBox >>= requireType(intType)
      } yield new Binary(b.operator, left, right, intType.name) at b

      case "=" | "#" | "<" | "<=" | ">" | ">=" => for {
        left  <- expression(b.left) >>= unBox >>= requireType(intType)
        right <- expression(b.right) >>= unBox >>= requireType(intType)
      } yield new Binary(b.operator, left, right, boolType.name) at b
    }
      
    case l: Literal => success(l)
      
    case n: New => for {
      t <- resolveClass(n.typed)      
    } yield n
      
    case a: Access => for {
      left  <- expression(a.left) >>= box
      right <- resolveMember(left.typed, a.right)      
    } yield new Access(
      left, 
      Name(
        right.name,
        right.typed,
        !right.isInstanceOf[Method]),
      right.typed,
      !right.isInstanceOf[Method]) at a
      
    case n: Name => for {
      d    <- resolve(n)
      _    <- require(!isInstanceOf[Class]) (Error(n.pos, n.typed + " is a class"))        
      r    <- d match {
        case m: Member => for {
            c <- currentType      
            l <- expression(Name("SELF", c)) >>= box
          } yield Access(l, Name(n.name, m.typed) at n, m.typed)
        case Variable(_,t,_) => success(Name(n.name, t, true) at n)
    }} yield r

    case x => success(x)
  } 

  def box(expr: Expression): Transform[Expression] = for {
    t2   <- resolveClass(expr.typed)
  } yield Class.box.get(t2).map(t => Box(expr, t.name))
               .getOrElse(if (expr.lvalue) DeRef(expr, expr.typed) else expr)

  def unBox(expr: Expression): Transform[Expression] = for {
    t2   <- resolveClass(expr.typed)    
    r    <- if (expr.lvalue) unBox(DeRef(expr, expr.typed))
            else success(Class.unBox.get(t2).map(t => UnBox(expr, t.name)).getOrElse(expr))
  } yield r

  def requireType(t: Class)(expr: Expression): Transform[Expression] = for {
    t2     <- resolveClass(expr.typed)
    _      <- require(t == t2) (Error(expr.pos, "type mismatch\n    expected: %s\n    found: %s".format(t.name, expr.typed)))
  } yield expr
}