package de.martinring.oopsc

import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position
import de.martinring.oopsc.ast._
import javax.management.remote.rmi._RMIConnection_Stub

/*
 * Transform Monad combining a state monad carrying the declaration table and an error collection monad.
 * @author Martin Ring
 */
trait Transform[S,A] {
  import Transform._

  def apply(s: S, decls: Declarations): Failable[(S, Declarations, A)]

  def map[B](f: A => B): Transform[B] = transform(
    apply(_).map{ case (s,d,a) => (s,d,f(a)) })

  def flatMap[B](f: A => Transform[B]): Transform[B] = transform(
    apply(_).flatMap { case (s,d,a) => f(a)(s,d) })
}

object Transform {
  def transform[S,A](f: (S, Declarations) => Failable[(S, Declarations, A)]) =
    new Transform[S,A] { def apply(s: S, decls: Declarations) = f(s, decls) }

  def withDecls(f: Declarations => Failable[Declarations]) = transform[_,Unit](
    (s,decls) => f(decls).map(decls => (s,decls,()))
  )

  def enter(decl: Declaration, decls: List[Declaration] = Nil) = withDecls(
    _.enter(decl.name).bind(decls))

  def leave = withDecls(
    _.leave)

  def bind(decl: Declaration): Transform[Unit] =
    bind(List(decl))

  def bind(decls: List[Declaration]) = withDecls(
    _.bind(decls))

  def rebind(decl: Declaration): Transform[Unit] =
    rebind(List(decl))

  def rebind(decls: List[Declaration]) = withDecls(
    _.rebind(decls)
  )

  def resolve(name: Name) = transform(
    (s, decls) => decls.apply(name).map((s,decls,_)))

  def require(f: Boolean)(msg: Message) = transform(
    (s, decls) => if(f) Success((s,decls,())) else Failure(List(msg)))

  def success[T](v: T) = transform(
    (s, decls) => Success((s,decls,v)))

  def sequence[T](l: List[Transform[T]]): Transform[List[T]] = 
    if (l.isEmpty) transform((s,decls) => Success((s, decls), Nil: List[T]))
    else l.tail.foldLeft(l.head.map(x => List(x))){
      case (a,b) => a.flatMap(x => b.map(x ++ List(_)))
    }

  def merge[T](l: List[Transform[T]]): Transform[List[T]] = transform[List[T]]{
    s: Declarations => Failable.merge[T](l.map(_.apply(s).map(_._2))).map((s,_))
  }
}