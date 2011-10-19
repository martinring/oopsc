package de.martinring.oopsc

import de.martinring.oopsc.ast._
import scala.util.parsing.input.NoPosition
import de.martinring.oopsc.Failable._

/*
 * Double linked declaration tree
 * @author Martin Ring
 * @param name the name of the scope
 * @param level the declarations defined on this level
 * @param parent an optional value containing the parent scope (None if this is top level)
 * @param children the child scopes
 */
case class Declarations(
  name: String,
  level: Map[String, Declaration] = Map.empty, 
  parent: Option[Declarations] = None, 
  children: Map[String, Declarations] = Map.empty) {  
  
  /* 
   * enter a new or existing named scope
   * @param n name of the scope
   */
  def enter(n: String) = 
    children.getOrElse(n, Declarations(name = n)).copy(parent = Some(this))
  
  /* 
   * leave the current scope and return to parent. Fails if this is the top level
   * scope
   */
  def leave: Failable[Declarations] = 
    if (parent.isDefined) Success(parent.get match {
      case parent => Declarations(parent.name, parent.level, parent.parent, parent.children + (name -> this))
    }) else 
      Failure(List(Error(NoPosition, "trying to leave top level scope")))

  /*
   * bind a declaration in the current scope. Fails if a declaration with the same
   * name is defined on the same level
   * @param decl the daclaration to bind
   */
  def bind(decl: Declaration): Failable[Declarations] =
    for {
      _ <- require(!level.isDefinedAt(decl.name)) (Error(decl.pos, "'%s' is allready defined on the same level".format(decl.name)))
    } yield copy(level = level + (decl.name -> decl))

  /*
   * bind a list of declarations. Equivalent to sequential @see bind for each list item.
   * @param decls the list of declarations to bind
   */
  def bind(decls: List[Declaration]): Failable[Declarations] = 
    decls.foldLeft(success(this)){ case (result,decl) => result.flatMap(_.bind(decl)) }
  
  /*
   * rebind a list of declarations. existing declarations with the same name will be
   * overwritten
   * @param decls the list of declarations to rebind
   */
  def rebind(decls: List[Declaration]): Failable[Declarations] =
    success(copy(level = level ++ decls.map(x => x.name -> x)))
  
  /*
   * find a declaration by its name. fails if no declaration with that name is defined.
   * @param name the name of the declaration to look for
   */
  def apply(name: Name): Failable[Declaration] = level.get(name.name) match {
    case Some(x) => success(x)
    case None => parent match {
        case Some(x) => x(name)
        case None => Failure(List(Error(name.pos, name.name + " is not in scope")))}
  }
}

/*
 * Companion Object for convenient creation of @see Declarations
 */
object Declarations {
  def apply(decls: Declaration*): Declarations = 
    Declarations("top level", Map(decls.map(x => x.name -> x): _*))
}