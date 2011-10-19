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

/*
 * Companion object with functions for the @see Transform monad.
 */
object Transform {
  /* build a transform monad */
  def transform[A](f: Context => Failable[(Context, A)]) =
    new Transform[A] { def apply(c: Context) = f(c) }
    
  def withDecls(f: Declarations => Failable[Declarations]) = transform[Unit](
    c => f(c.declarations).map(x => (c.copy(declarations = x), ())))

  /* returns the current type */
  val currentType = transform[String]( c => Success((c,c.currentType)) )
  
  /* returns the current method */
  val currentMethod = transform[String]( c => Success((c,c.currentMethod)) )
  
  /* adds @param size to the offset and returns the old value */
  def incOffset(size: Int) = transform[Int]( c => Success( (c.copy(counter = c.counter + size),c.counter) ))
                                      
  /* returns a unique label */
  val nextLabel = transform[String]( c => Success(
      (c.copy(counter = c.counter + 1), c.currentType + "_" + c.currentMethod + "_" + c.counter)))
  
  /* enter a new or existing scope. resets the offset counter */
  def enter(decl: Declaration, decls: List[Declaration] = Nil) = {
    val base = transform(c => c.declarations.enter(decl.name).bind(decls).map(x => (c.copy(declarations = x, counter = 0), ())))
    if (decl.isInstanceOf[Class]) 
      base >>= (_ => transform[Unit](c => Success((c.copy(currentType = decl.name),()))))
    else if (decl.isInstanceOf[Method])
      base >>= (_ => transform[Unit](c => Success((c.copy(currentMethod = decl.name),()))))
    else 
      base
  } 
  
  /* leave the current scope and return to parent */
  val leave = transform[Unit](
    c => c.declarations.leave.map(x => (c.copy(declarations = x, counter = 0), ())))

  /* monadic wrapper for bind in @see Declarations */
  def bind[S](decl: Declaration): Transform[Unit] =
    bind(List(decl))

  /* monadic wrapper for bind in @see Declarations */
  def bind(decls: List[Declaration]) = withDecls(
    _.bind(decls))

  /* monadic wrapper for rebind in @see Declarations */
  def rebind[S](decl: Declaration): Transform[Unit] =
    rebind(List(decl))

  /* monadic wrapper for rebind in @see Declarations */  
  def rebind(decls: List[Declaration]) = withDecls(
    _.rebind(decls))

  /* monadic wrapper for apply in @see Declarations */  
  def resolve[S](name: Name) = transform[Declaration](
    c => c.declarations(name).map((c,_)))
  
  /* monadic wrapper for apply in @see Declarations. fails if result is not of type @see Class */  
  def resolveClass(name: Name): Transform[Class] = for {
    c <- resolve(name)
    _ <- require(c.isInstanceOf[Class]) (Error(name.pos, name.name + " is not a class"))
  } yield c match { case c: Class => c }    

  /* monadic wrapper for apply in @see Declarations. fails if result is not of type @see Member */  
  def resolveMember(typed: String, name: Name): Transform[Member] = for {
    c <- resolveClass(typed)
    val m = c.members.find(_.name == name.name)
    _ <- require(m.isDefined)           (Error(name.pos, name.name + " is not a member of type " + typed))
  } yield m.get  
  
  /* monadic wrapper for apply in @see Declarations. fails if result is not of type @see Variable */    
  def resolveVariable(name: Name): Transform[Variable] = for {
    v <- resolve(name)
    _ <- require(v.isInstanceOf[Variable]) (Error(name.pos, name.name + " is not a variable"))
  } yield v match { case v: Variable => v }

  /* fail if @param f is false with message @param msg */
  def require(f: Boolean)(msg: Message) = transform(
    c => if(f) Success((c,())) else Failure(List(msg)))  
  
  /* mzero */
  def success[T](v: T) = transform(
    c => Success((c,v)))

  /* fail with @param msg */
  def fail(msg: Message) = transform(
    c => Failure(List(msg)))      

  /* 
   * sequence monadic operations in a list.
   */
  def sequence[T](l: List[Transform[T]]): Transform[List[T]] =    
    if (l.isEmpty) transform(c => Success((c,Nil))) 
    else l.tail.foldLeft { l.head map (List(_)) } {
      case (a, b) => for {
        first  <- a
        second <- b
      } yield first ++ List(second)
    }

  /* 
   * merge monadic operations in a list. note that this means, the context is 
   * lost. but can be run in parallel
   */
  def merge[T](l: List[Transform[T]]): Transform[List[T]] = transform[List[T]](
    c => Failable.merge[T](l.map(_.apply(c).map(_._2))).map((c,_)))
}