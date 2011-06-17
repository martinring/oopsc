package de.martinring.oopsc

import de.martinring.oopsc.ast._
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position

/*
 * Declaration table
 * @author Martin Ring
 */
case class Declarations(
  name: String,
  level: Map[String, Declaration] = Map.empty, 
  parent: Option[Declarations] = None, 
  children: Map[String, Declarations] = Map.empty) {
  
  def enter(n: String) = 
    children.getOrElse(n, Declarations(name = n, parent = Some(this)))
  
  def leave: Failable[Declarations] = 
    if (parent.isDefined) Success(parent.get match {
      case parent => Declarations(parent.name, parent.level, parent.parent, parent.children + (name -> this))
    }) else 
      Failure(List(Error(NoPosition, "trying to leave top level scope")))
  
  def bind(decls: List[Declaration]): Failable[Declarations] = 
    decls.foldLeft(Success(this): Failable[Declarations]){case (result, decl) =>
      result.flatMap{scope => if (scope.level.isDefinedAt(decl.name))
        Errors(scope, List(Error(decl.pos, "'%s' is allready defined on the same level".format(decl.name))))
      else 
        Success(scope.copy(level = scope.level + (decl.name -> decl)))}
  }
  
  def rebind(decls: List[Declaration]): Failable[Declarations] =
    Success(copy(level = level ++ decls.map(x => x.name -> x)))

  def apply(name: Name): Failable[Declaration] = level.get(name.name) match {
    case Some(x) => Success(x)
    case None => parent match {
        case Some(x) => x(name)
        case None => Failure(List(Error(name.pos, name.name + " is not in scope")))}
  }
}

object Scope{
  def apply(decls: Declaration*): Declarations = 
    Declarations("top level", Map(decls.map(x => x.name -> x): _*))
}