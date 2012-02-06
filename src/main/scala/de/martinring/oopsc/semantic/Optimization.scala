///*
// * To change this template, choose Tools | Templates
// * and open the template in the editor.
// */
//
//package de.martinring.oopsc
//
//import de.martinring.oopsc.ast._
//import de.martinring.oopsc.ast.Patterns._
//import de.martinring.oopsc.Transform._
//
//object Optimization {
//  def optimize(p: Program): Transform[Program] = for {
//    classes <- sequence(p.classes map optimize)
//  } yield Program(classes) at p
//
//  def optimize(c: Class): Transform[Class] = for {
//    methods    <- sequence(c.methods map optimize)
//    result     <- update(c.copy(methods = methods) at c)
//  } yield result
//
//  def optimize(m: Method): Transform[Method] = for {
//    result     <- update(m.copy(body = (m.body map optimize) flatten) at m)
//  } yield result
//
//  def optimize(st: Statement): List[Statement] = st match {
//    case w: Write =>
//      List(Write(optimize(w.operand)))
//
//    case w: While =>
//      lazy val body = (w.body map optimize).flatten
//      optimize(w.condition) match {
//        case Bool(false) => Nil // while false => _
//        case cond => List(While(cond, body))
//      }
//
//    case i: If =>
//      lazy val body = (i.body map optimize).flatten
//      lazy val elseBody = (i.elseBody map optimize).flatten
//      optimize(i.condition) match {
//        case Bool(true) => body // if true then else => then
//        case Bool(false) => elseBody // if false then else => else
//        case cond => List(If(cond, body, elseBody))
//    }
//
//    case c: Call =>
//      List(Call(optimize(c.call)))
//
//    case a: Assign =>
//      List(Assign(optimize(a.left), optimize(a.right)))
//
//    case r@Return(Some(expr)) =>
//      List(Return(Some(optimize(expr))))
//
//    case statement => List(statement)
//  }
//
//  def optimize(e: Expression): Expression = e match {
//    case Minus(Minus(x)) => optimize(x)
//    case Int(0) + x => optimize(x)
//    case x + Int(0) => optimize(x)
//    case b@(x + Minus(y)) => Binary("-", optimize(x), optimize(y), Class.intType.name)
//    case Int(0) - x => Unary("-", optimize(x), Class.intType.name)
//    case x - Int(0) => optimize(x)
//    case x - Minus(y) => Binary("+", optimize(x), optimize(y), Class.intType.name)
//
//    case Int(0) * x => Literal(0, Class.intType.name)
//    case x * Int(0) => Literal(0, Class.intType.name)
//    case Int(1) * x => optimize(x)
//    case x * Int(1) => optimize(x)
//
//
//    case expr => expr
//  }
//}
