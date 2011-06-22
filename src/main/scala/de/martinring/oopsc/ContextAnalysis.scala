/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.martinring.oopsc

import de.martinring.oopsc.ast._
import de.martinring.oopsc.ast.Class._
import de.martinring.oopsc.Transform._
import java.lang.Boolean

case class Context(declarations: Declarations, size: Int)

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
    _          <- bind(Variable("SELF", Name(c.name)))
    attributes <- merge(c.attributes.map(attribute(_)))
    methods    <- merge(c.methods.map(method(_)))
    _          <- rebind(methods)
    _          <- leave
  } yield Class(c.name, attributes ++ methods) at c
  
  def attribute(a: Attribute): Transform[Attribute] = for {
    _ <- resolveClass(a.typed)
  } yield a
  
  def variable(v: Variable): Transform[Variable] = for {
    _ <- resolveClass(v.typed)    
  } yield v
  
  def method(m: Method): Transform[Method] = for {
    _         <- enter(m, m.variables)
    variables <- merge(m.variables.map(variable(_)))
    _         <- rebind(variables)
    body      <- merge(m.body.map(statement(_)))                 
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
      body      <- merge(w.body.map(statement(_)))
    } yield While(condition, body) at w
      
    case i: If => for {
      condition <- expression(i.condition) >>= unBox >>= requireType(boolType)
      body      <- merge(i.body.map(statement(_)))
    } yield If(condition, body) at i
    
    case c: Call => for {
      call <- expression(c.call)      
    } yield Call(call) at c
    
    case a: Assign => for {
      left  <- expression(a.left)
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
      left  <- expression(a.left)
      right <- resolveMember(left.typed, a.right)
    } yield new Access(left, a.right, right.typed) at a
      
    case n: Name => for {
      d    <- resolve(n)
      _    <- require(!isInstanceOf[Class]) (Error(n.pos, n.typed + " is a class"))
      val typed: String = d match {
        case Variable(_,t) => t
        case m: Member => m.typed }      
      val name = new Name(n.name, typed) at n
      val node = d match {
        case _: Attribute => Access(Name("SELF"), name, typed)
        case _ => name
      }
    } yield node

    case x => success(x)
  } 

  def resolveClass(name: Name): Transform[Class] = for {
    c <- resolve(name)
    _ <- require(c.isInstanceOf[Class]) (Error(name.pos, name.name + " is not a class"))
  } yield c match { case c: Class => c }    
  
  def resolveMember(typed: String, name: Name): Transform[Member] = for {
    c <- resolveClass(typed)
    val m = c.members.find(_.name == name.name)
    _ <- require(m.isDefined)           (Error(name.pos, name.name + " is not a member of type " + typed))
  } yield m.get

  def box(expr: Expression): Transform[Expression] = for {
    t2   <- resolveClass(expr.typed)
    lv   <- lValue(expr)
  } yield Class.box.get(t2).map(t => Box(expr, t.name))
               .getOrElse(if (lv) DeRef(expr, expr.typed) else expr)

  def unBox(expr: Expression): Transform[Expression] = for {
    t2   <- resolveClass(expr.typed)
    lv   <- lValue(expr)
    r    <- if (lv) unBox(DeRef(expr, expr.typed))
            else success(Class.unBox.get(t2).map(t => UnBox(expr, t.name)).getOrElse(expr))
  } yield r

  def requireType(t: Class)(expr: Expression): Transform[Expression] = for {
    t2     <- resolveClass(expr.typed)
    _      <- require(t == t2) (Error(expr.pos, "type mismatch\n    expected: %s\n    found: %s".format(t.name, expr.typed)))
  } yield expr

  def lValue(expr: Expression): Transform[Boolean] = expr match {
    case n: Name => for {
        decl <- resolve(n)
      } yield decl match {
        case _:Attribute => true
        case _:Variable => true
        case _ => false
      }
    case _ => success(false)
  }
}