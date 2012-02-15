package de.martinring.oopsc.semantic

import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.output._
import de.martinring.oopsc._
import de.martinring.util._
import de.martinring.util.Failable._

/**
 * Context carrying around information about declarations, the current position
 * in the declaration tree, name aliases and virtual method tables.
 * @author Martin Ring
 */
case class Context(path:         List[String] = Nil,
                   aliases:      Map[List[String], List[String]] = Map(),
                   declarations: Map[List[String], Declaration] = Map(),
                   vmts:         Map[AbsoluteName, List[AbsoluteName]] = Map())

/*
 * Transform monad combining a state monad carrying a [[de.martinring.oopsc.semantic.Context]]
 * with an error collection monad.
 * @author Martin Ring
 */
trait Transform[A] {
  import Transform._  
    
  /** Run the monad 
   * @param context The current context. */
  def apply(context: Context = Context()): Failable[(Context, A),Message]
  
  /** Modify result without altering State */
  def map[B](f: A => B): Transform[B] = transform(
    apply(_).map{ case (c,a) => (c,f(a)) })

  /** Monadic bind operation. */
  def flatMap[B](f: A => Transform[B]): Transform[B] = transform(
    apply(_).flatMap { case (c,a) => f(a)(c) })

  /** Short operator for [[flatMap]] */
  def >>=[B](f: A => Transform[B]): Transform[B] = flatMap(f)

  /** >>= _ -> */
  def >>[B](f: => Transform[B]): Transform[B] = flatMap(_ => f)

  /** If errors were thrown, try an alternative transform.
   * @param f the alternative to try */
  def or(f: Transform[A]): Transform[A] = transform(
    c => apply(c) match {
      case s: Success[A,Message] => s
      case _ => f(c)
    })

  /** If errors were thrown ignore the result and give back a default value.
   * This results in a guaranteed [[de.martinring.util.Success]]
   * @param default the default value */
  def continueWith (default: A): Transform[A] = transform( c =>
    apply(c) match {
      case f: Failure[Message] => Errors((c, default), f.messages)
      case x => x
    }
  )

  /** If errors were thrown, ignore these and replace them with e
   * @param e the message to be thrown */
  def ! (e: Message): Transform[A] = transform( apply(_) match {
      case f: Failure[Message] => f.copy(messages = List(e))
      case f: Errors[(Context,A),Message] => f.copy(messages = List(e))
      case x => x
    } )
}

/**
 * Companion object with combinator functions for the [[de.martinring.oopsc.semantic.Transform]]
 * monad.
 * @author Martin Ring
 */
object Transform {
  /** build a transform monad */
  def transform[A](f: Context => Failable[(Context, A),Message]) =
    new Transform[A] { def apply(c: Context) = f(c) }

  /** returns a resolved identifier pointing to the current type */
  val currentClass = transform[AbsoluteName] { c =>
      Success((c, c.path.prefixes.reverse.collectFirst {
        case prefix if c.declarations.isDefinedAt(prefix) && c.declarations(prefix).isInstanceOf[Class] =>
          new AbsoluteName(prefix)
      }.getOrElse(new AbsoluteName(List("_init")))))
    }

  /** returns a resolved identifier pointing to the current method */
  val currentMethod = transform[Option[AbsoluteName]] { c =>
    Success((c, c.path.prefixes.reverse.collectFirst {
      case prefix if c.declarations(prefix).isInstanceOf[Method] =>
        Some(new AbsoluteName(prefix))
    }.getOrElse(None)))
  }

  /** returns the current path */
  val path =
    transform( c => Success((c, c.path)) )

  /** Enter a relative scope */
  def enter[A](name: String)(f: Transform[A]): Transform[A] = for {
    p <- transform { c => Success((c.copy(path = c.path :+ name), c.path)) }
    x <- f
    _ <- transform { c => Success((c.copy(path=p), ())) }
  } yield x

  /** Enter an absolute scope */
  def enter[A](id: Name)(f: Transform[A]): Transform[A] = id match {
    case r: AbsoluteName => for {
      p <- transform { c => Success((c.copy(path=r.path), c.path)) }
      x <- f
      _ <- transform { c=> Success((c.copy(path=p), ())) }
    } yield x
    case _ => sys.error("unresolved identifier")
  }

  /** Create an alias for a declaration in the current scope. (i.e. to point to
    * methods of the parent class) */
  def createAlias(decl: Declaration): Transform[Unit] = transform { c =>
    val p = c.path :+ decl.name.relative
    decl.name match {
      case AbsoluteName(path,_) => Success((c.copy(aliases = c.aliases.updated(p,path)),()))
      case _ => sys.error("Should not happen")
    }
    
  }
    
  /** Bind the declaration in the current scope. If the relative Name exists allready an
   * error will be thrown. */
  def bind(decl: Declaration): Transform[AbsoluteName] = transform { c =>    
    val p = c.path :+ decl.name.relative
    c.declarations.get(p) match {
      case None    => Success( (c.copy(declarations = c.declarations.updated(p,decl)), new AbsoluteName(p)) )
      case Some(x: Declaration) => Failable.fail(Error(decl.pos,
          if (Class.predefined.exists(_.name.relative == decl.name.relative)) decl.name + " is already defined" 
          else decl.name + " is already defined at " + x.pos))
    }
  }

  /** Bind multiple declarations in the current scope. (preserves order) */
  def bind(decls: List[Declaration]): Transform[List[AbsoluteName]] =
    sequence(decls map bind)

  /** Update an existing binding of the supplied declaration */
  def update[T <: Declaration](decl: T): Transform[T] = transform { c =>    
    val p = decl.name match {
      case r: AbsoluteName => r.path
      case _ => sys.error("unresolved identifier " + decl.name)
    }
    Success( (c.copy(declarations = c.declarations.updated(p,decl)), decl) )
  }

  /** Resolve a name and return an absolute identifier. Throws errors if the name
   * is not in scope */
  def resolve(name: Name) = transform[AbsoluteName]{ c =>
    c.path.prefixes.toList.reverse.collectFirst {
      case prefix if c.declarations.isDefinedAt(prefix :+ name.relative) =>
        (c, new AbsoluteName(prefix :+ name.relative) at name) 
      case prefix if c.aliases.isDefinedAt(prefix :+ name.relative) =>
        (c, new AbsoluteName(prefix :+ name.relative) at name) 
    } orFail (Error(name.pos, name.relative + " is not in scope"))
  }

  /** Resolve a class name */
  def resolveClassName(name: Name) = for {
    n <- resolve(name)
    _ <- getType(n)
  } yield n

  /** Get the current declaration which is bound to the name */          
  def get(name: Name) = transform[Declaration]{ c =>    
    name match {
      case i: AbsoluteName => c.declarations.get(i.path) match {
          case None => c.aliases.get(i.path) match {
              case None => fail(Error(i.pos, "not found: " + i.path.mkString(".")))
              case Some(a) => Success((c,c.declarations(a)))
          }
          case Some(d) => Success((c, d))
        }
      case _ => Failure(List()) // this is a recursive error that does not need to
                                // be reported again, so we supply an empty error
                                // list.
    }
  }

  /** [[get]] for classes */
  def getType(name: Name): Transform[Class] = for {
    c <- get(name)
    _ <- require(c.isInstanceOf[Class]) (Error(name.pos, name.relative + " is not a class"))
  } yield c match { case c: Class => c }

  /** Get the current vmt which is bound to the name */         
  def getVMT(name: Name): Transform[List[AbsoluteName]] = transform{ c =>
    Success((c,c.vmts.getOrElse(name.asInstanceOf[AbsoluteName], Nil))) }

  /** Update the vmt binding for class with [[name]] */
  def setVMT(name: Name)(vmt: List[AbsoluteName]): Transform[Unit] = transform{ c =>
    Success((c.copy(vmts = c.vmts.updated(name.asInstanceOf[AbsoluteName], vmt)), ())) }

  /** [[get]] for methods */
  def getMethod(name: Name): Transform[Method] = for {
    c <- get(name)
    _ <- require(c.isInstanceOf[Method]) (Error(name.pos, name.relative + " is not a method"))
  } yield c match { case c: Method => c }

  /** [[get]] for variables */
  def getVariable(name: Name): Transform[Variable] = for {
    v <- get(name)
    _ <- require(v.isInstanceOf[Variable]) (Error(name.pos, name.relative + " is not a variable"))
  } yield v match { case v: Variable => v }

  /** throw msg if @param f is false */
  def require(f: Boolean)(msg: => Message) = transform(
    c => if(f) Success((c,())) else Failure(List(msg)))

  /** throw msg if @param f is true */
  def throwIf(f: Boolean)(msg: => Message) = transform(
    c => if(f) Errors((c,()),List(msg)) else Success((c,())))

  def error(msg: => Message) = transform[Nothing]{
    c => Errors((c,sys.error("should never be evaluated")), List(msg))
  }
            
  /** sequence monadic operations in a list. */
  def sequence[T](l: List[Transform[T]]): Transform[List[T]] =
    if (l.isEmpty) transform(c => Success((c,Nil)))
    else l.tail.foldLeft { l.head map (List(_)) } {
      case (a, b) => for {
        first  <- a continueWith Nil
        second <- b.map(List(_)) continueWith Nil
      } yield first ++ second
    }

  /** return a value ignoring previous value and state */
  def just[A](v: A) = transform(c => Success((c, v)))
}