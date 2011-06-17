/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.martinring.oopsc

import de.martinring.oopsc.ast._
import de.martinring.oopsc.Transform._

object ContextAnalysis {     
  def program(p: Program): Transform[Program] = for {   
    _    <- bind(p.main)    
    _    <- check(p.main.name == "Main")                   (p.main.pos, "The class has to be named Main")
    _    <- check(p.main.methods.exists(_.name == "main")) (p.main.pos, "Main class doesn't contain a main method")
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
      operand <- expression(w.operand)
    } yield Write(operand) at w
      
    case w: While => for {
      condition <- expression(w.condition)
      body <- merge(w.body.map(statement(_)))
    } yield While(condition, body) at w
      
    case i: If => for {
      condition <- expression(i.condition)
      body <- merge(i.body.map(statement(_)))
    } yield If(condition, body) at i
    
    case c: Call => for {
      call <- expression(c.call)      
    } yield Call(call) at c
    
    case a: Assign => for {
      left <- expression(a.left)
      right <- expression(a.right)
    } yield Assign(left, right) at a      
  }  
  
  def expression(e: Expression): Transform[Expression] = e match {       
    case u: Unary => for {
      operand <- expression(u.operand)
    } yield new Unary(u.operator, operand, operand.typed) at u
      
    case b: Binary => for {
      left  <- expression(b.left)
      right <- expression(b.right)      
    } yield new Binary(b.operator, left, right, left.typed) at b
      
    case l: Literal => success(l)
      
    case n: New => for {
      t <- resolveClass(n.typed)      
    } yield n
      
    case a: Access => for {
      left  <- expression(a.left)
      right <- resolveMember(left.typed, a.right)
    } yield new Access(left, a.right, right.typed) at a
      
    case n: Name => for {
      d <- resolve(n)
      _ <- check(!isInstanceOf[Class])(n.pos, n.typed + " is a class")
      val typed: String = d match {
        case Variable(_,t) => t
        case m: Member => m.typed }      
      val name = new Name(n.name, typed) at n
      val node = d match {
        case _: Attribute => Access(Name("SELF"), name, typed)
        case _ => name
      }
    } yield node
  } 

  def resolveClass(name: Name): Transform[Class] = for {
    c <- resolve(name)
    _ <- check(c.isInstanceOf[Class])(name.pos, name.name + " is not a class")
  } yield c match { case c: Class => c }    
  
  def resolveMember(typed: String, name: Name): Transform[Member] = for {
    c <- resolveClass(typed)
    val m = c.members.find(_.name == name.name)
    _ <- check(m.isDefined)(name.pos, name.name + " is not a member of type " + typed)
  } yield m.get    
}