package de.martinring.oopsc

import de.martinring.oopsc.ast._
import scala.util.parsing.input.NoPosition
import de.martinring.oopsc.Failable._

/*
 * Declaration table
 * @author Martin Ring
 */
case class Declarations(
  name: String,
  level: Map[String, Declaration] = Map.empty, 
  parent: Option[Declarations] = None, 
  children: Map[String, Declarations] = Map.empty) {
  
  def top: Declarations = parent.map(_.top) getOrElse this      
  
  def enterType(n: String) = 
    top.children.get(n).map(_.copy(parent = Some(this))) orFail Error(NoPosition, "Unknown Type")
  
  def enter(n: String) = 
    children.getOrElse(n, Declarations(name = n, parent = Some(this)))
  
  def leave: Failable[Declarations] = 
    if (parent.isDefined) Success(parent.get match {
      case parent => Declarations(parent.name, parent.level, parent.parent, parent.children + (name -> this))
    }) else 
      Failure(List(Error(NoPosition, "trying to leave top level scope")))

  def bind(decl: Declaration): Failable[Declarations] =
    for {
      _ <- require(!level.isDefinedAt(decl.name)) (Error(decl.pos, "'%s' is allready defined on the same level".format(decl.name)))
    } yield copy(level = level + (decl.name -> decl))

  def bind(decls: List[Declaration]): Failable[Declarations] = 
    decls.foldLeft(success(this)){ case (result,decl) => result.flatMap(_.bind(decl)) }
  
  def rebind(decls: List[Declaration]): Failable[Declarations] =
    success(copy(level = level ++ decls.map(x => x.name -> x)))
  
  def apply(name: Name): Failable[Declaration] = level.get(name.name) match {
    case Some(x) => success(x)
    case None => parent match {
        case Some(x) => x(name)
        case None => Failure(List(Error(name.pos, name.name + " is not in scope")))}
  }
}

object Declarations {
  def apply(decls: Declaration*): Declarations = 
    Declarations("top level", Map(decls.map(x => x.name -> x): _*))
}