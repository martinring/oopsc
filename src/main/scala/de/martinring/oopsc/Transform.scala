package de.martinring.oopsc

import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position
import de.martinring.oopsc.ast._
import javax.management.remote.rmi._RMIConnection_Stub

/*
 * Transform monad combining a state monad carrying the declaration table and an error collection monad.
 * @author Martin Ring
 */
trait Transform[A] {
  import Transform._

  def apply(context: Context = Context(Declarations(), "none")): Failable[(Context, A)]

  def map[B](f: A => B): Transform[B] = transform(
    apply(_).map{ case (c,a) => (c,f(a)) })

  def flatMap[B](f: A => Transform[B]): Transform[B] = transform(
    apply(_).flatMap { case (c,a) => f(a)(c) })

  def >>=[B](f: A => Transform[B]): Transform[B] = flatMap(f)
}

object Transform {
  def transform[A](f: Context => Failable[(Context, A)]) =
    new Transform[A] { def apply(c: Context) = f(c) }

  def withDecls(f: Declarations => Failable[Declarations]) = transform[Unit](
    c => f(c.declarations).map(x => (c.copy(declarations = x), ())))

  def currentType = transform[String]( c => Success((c,c.currentType)) )
  
  def enter(decl: Declaration, decls: List[Declaration] = Nil) = {
    val base = withDecls(_.enter(decl.name).bind(decls)) 
    if (decl.isInstanceOf[Class]) 
      base >>= (_ => transform[Unit](c => Success((c.copy(currentType = decl.name),()))))
    else 
      base
  } 

  def enterType(decl: Class) = withDecls(
    _.enterType(decl.name)
  )
  
  def leave = withDecls(
    _.leave)

  def bind[S](decl: Declaration): Transform[Unit] =
    bind(List(decl))

  def bind(decls: List[Declaration]) = withDecls(
    _.bind(decls))

  def rebind[S](decl: Declaration): Transform[Unit] =
    rebind(List(decl))

  def rebind(decls: List[Declaration]) = withDecls(
    _.rebind(decls))

  def resolve[S](name: Name) = transform[Declaration](
    c => c.declarations.apply(name).map((c,_)))
  
  def resolveClass(name: Name): Transform[Class] = for {
    c <- resolve(name)
    _ <- require(c.isInstanceOf[Class]) (Error(name.pos, name.name + " is not a class"))
  } yield c match { case c: Class => c }    

  def resolveMember(typed: String, name: Name): Transform[Member] = for {
    c <- resolveClass(typed)
    val m = c.members.find(_.name == name.name)
    _ <- require(m.isDefined)           (Error(name.pos, name.name + " is not a member of type " + typed))
  } yield m.get  
  
  def resolveVariable(name: Name): Transform[Variable] = for {
    v <- resolve(name)
    _ <- require(v.isInstanceOf[Variable]) (Error(name.pos, name.name + " is not a variable"))
  } yield v match { case v: Variable => v }

  def require(f: Boolean)(msg: Message) = transform(
    c => if(f) Success((c,())) else Failure(List(msg)))  
  
  def success[T](v: T) = transform(
    c => Success((c,v)))

  def fail(msg: Message) = transform(
    c => Failure(List(msg)))      

/*
  def sequence[T](l: List[Transform[T]]): Transform[List[T]] =
    if (l.isEmpty) transform(c => Success((c, Nil: List[T])))
    else l.tail.foldLeft(l.head.map(List(_)){
      case (a,b) => a.flatMap(x => b.map(x ++ List(_))) })
*/

  def merge[T](l: List[Transform[T]]): Transform[List[T]] = transform[List[T]](
    c => Failable.merge[T](l.map(_.apply(c).map(_._2))).map((c,_)))
}