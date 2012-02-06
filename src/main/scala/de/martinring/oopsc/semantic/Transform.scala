package de.martinring.oopsc

import de.martinring.oopsc.syntactic._
import de.martinring.util._
import de.martinring.util.Failable._

/**
 * Context carrying around information about declarations and the current position
 * in the declaration tree.
 */
case class Context(path:         List[String] = Nil,
                   aliases:      Map[List[String], List[String]] = Map(),
                   declarations: Map[List[String], Declaration] = Map(),
                   vmts:         Map[AbsoluteName, List[AbsoluteName]] = Map())

/*
 * Transform monad combining a state monad carrying the declaration table and an error collection monad.
 * @author Martin Ring
 */
trait Transform[A] {
  import Transform._  
  
  def apply(context: Context = Context()): Failable[(Context, A),Message]

  def map[B](f: A => B): Transform[B] = transform(
    apply(_).map{ case (c,a) => (c,f(a)) })

  def flatMap[B](f: A => Transform[B]): Transform[B] = transform(
    apply(_).flatMap { case (c,a) => f(a)(c) })

  /**
   * Short operator for @see flatMap
   */
  def >>=[B](f: A => Transform[B]): Transform[B] = flatMap(f)

  def >>[B](f: => Transform[B]): Transform[B] = flatMap(_ => f)

  /**
   * If errors were thrown, try an alternative transform.
   * @param f the alternative to try
   */
  def or(f: Transform[A]): Transform[A] = transform(
    c => apply(c) match {
      case s: Success[A,Message] => s
      case _ => f(c)
    })

  /**
   * If errors were thrown ignore the result and give back a default value.
   * This results in a guaranteed @see Success
   * @param default the default value
   */
  def continueWith (default: A): Transform[A] = transform( c =>
    apply(c) match {
      case f: Failure[Message] => Errors((c, default), f.messages)
      case x => x
    }
  )

  /**
   * If errors were thrown, ignore these and replace them with e
   * @param e the message to be thrown
   */
  def ! (e: Message): Transform[A] = transform( apply(_) match {
      case f: Failure[Message] => f.copy(messages = List(e))
      case f: Errors[(Context,A),Message] => f.copy(messages = List(e))
      case x => x
    } )
}

/*
 * Companion object with functions for the @see Transform monad.
 */
object Transform {
  /* build a transform monad */
  def transform[A](f: Context => Failable[(Context, A),Message]) =
    new Transform[A] { def apply(c: Context) = f(c) }

  /* returns an resolved identifier pointing to the current type */
  val currentClass = transform[AbsoluteName] { c =>
      Success((c, c.path.prefixes.reverse.collectFirst {
        case prefix if c.declarations.isDefinedAt(prefix) && c.declarations(prefix).isInstanceOf[Class] =>
          new AbsoluteName(prefix)
      }.getOrElse(new AbsoluteName(List("_init")))))
    }

  /* returns an resolved identifier pointing to the current method */
  val currentMethod = transform[Option[AbsoluteName]] { c =>
    Success((c, c.path.prefixes.reverse.collectFirst {
      case prefix if c.declarations(prefix).isInstanceOf[Method] =>
        Some(new AbsoluteName(prefix))
    }.getOrElse(None)))
  }

  /* returns the current path */
  val path =
    transform( c => Success((c, c.path)) )

  /* Enter a declaration */
  def enter[A](name: String)(f: Transform[A]): Transform[A] = for {
    p <- transform { c => Success((c.copy(path = c.path :+ name), c.path)) }
    x <- f
    _ <- transform { c => Success((c.copy(path=p), ())) }
  } yield x

  def enter[A](id: Name)(f: Transform[A]): Transform[A] = id match {
    case r: AbsoluteName => for {
      p <- transform { c => Success((c.copy(path=r.path), c.path)) }
      x <- f
      _ <- transform { c=> Success((c.copy(path=p), ())) }
    } yield x
    case _ => sys.error("unresolved identifier")
  }

  def createAlias(decl: Declaration): Transform[Unit] = transform { c =>
    val p = c.path :+ decl.name.relative
    decl.name match {
      case AbsoluteName(path,_) => Success((c.copy(aliases = c.aliases.updated(p,path)),()))
      case _ => sys.error("Should not happen")
    }
    
  }
    
  /* monadic wrapper for bind in declarations */
  def bind(decl: Declaration): Transform[AbsoluteName] = transform { c =>
    val p = c.path :+ decl.name.relative
    c.declarations.get(p) match {
      case None    => Success( (c.copy(declarations = c.declarations.updated(p,decl)), new AbsoluteName(p)) )
      case Some(x: Declaration) => Failable.fail(Error(decl.pos,
          if (Class.predefined.exists(_.name.relative == decl.name.relative)) decl.name + " is allready defined" 
          else decl.name + " is allready defined at " + x.pos))
    }
  }

  /* monadic wrapper for bind in declarations */
  def bind(decls: List[Declaration]): Transform[List[AbsoluteName]] =
    sequence(decls map bind)

  /* monadic wrapper for rebind in declarations */
  def update[T <: Declaration](decl: T): Transform[T] = transform { c =>    
    val p = decl.name match {
      case r: AbsoluteName => r.path
      case _ => sys.error("unresolved identifier " + decl.name)
    }
    Success( (c.copy(declarations = c.declarations.updated(p,decl)), decl) )
  }

  /* monadic wrapper for apply in @see Declarations */
  def resolve(name: Name) = transform[AbsoluteName]{ c =>
    c.path.prefixes.toList.reverse.collectFirst {
      case prefix if c.declarations.isDefinedAt(prefix :+ name.relative) =>
        (c, new AbsoluteName(prefix :+ name.relative) at name) 
      case prefix if c.aliases.isDefinedAt(prefix :+ name.relative) =>
        (c, new AbsoluteName(prefix :+ name.relative) at name) 
    } orFail (Error(name.pos, name.relative + " is not in scope"))
  }

  def resolveClassName(name: Name) = for {
    n <- resolve(name)
    _ <- getType(n)
  } yield n

  def get(name: Name) = transform[Declaration]{ c =>    
    name match {
      case i: AbsoluteName => c.declarations.get(i.path) match {
          case None => c.aliases.get(i.path) match {
              case None => fail(Error(i.pos, "Not found: " + i.path.mkString(".")))
              case Some(a) => Success((c,c.declarations(a)))
          }
          case Some(d) => Success((c, d))
        }
      case _ => Failure(List()) // this is a recursive error that does not need to
                                // be reported again, so we supply an empty error
                                // list.
    }
  }

  /* monadic wrapper for apply in @see Declarations. fails if result is not of type @see Class */
  def getType(name: Name): Transform[Class] = for {
    c <- get(name)
    _ <- require(c.isInstanceOf[Class]) (Error(name.pos, name.relative + " is not a class"))
  } yield c match { case c: Class => c }

  def getVMT(name: Name): Transform[List[AbsoluteName]] = transform{ c =>
    Success((c,c.vmts.getOrElse(name.asInstanceOf[AbsoluteName], Nil))) }

  def setVMT(name: Name)(vmt: List[AbsoluteName]): Transform[Unit] = transform{ c =>
    Success((c.copy(vmts = c.vmts.updated(name.asInstanceOf[AbsoluteName], vmt)), ())) }

  /* monadic wrapper for apply in @see Declarations. fails if result is not of type @see Method */
  def getMethod(name: Name): Transform[Method] = for {
    c <- get(name)
    _ <- require(c.isInstanceOf[Method]) (Error(name.pos, name.relative + " is not a method"))
  } yield c match { case c: Method => c }

  /* monadic wrapper for apply in @see Declarations. fails if result is not of type @see Variable */
  def getVariable(name: Name): Transform[Variable] = for {
    v <- get(name)
    _ <- require(v.isInstanceOf[Variable]) (Error(name.pos, name.relative + " is not a variable"))
  } yield v match { case v: Variable => v }

  /* fail if @param f is false with message @param msg */
  def require(f: Boolean)(msg: => Message) = transform(
    c => if(f) Success((c,())) else Failure(List(msg)))

  /* warn if @param f is false with message @param msg */
  def throwIf(f: Boolean)(msg: => Message) = transform(
    c => if(f) Errors((c,()),List(msg)) else Success((c,())))

  /* sequence monadic operations in a list. */
  def sequence[T](l: List[Transform[T]]): Transform[List[T]] =
    if (l.isEmpty) transform(c => Success((c,Nil)))
    else l.tail.foldLeft { l.head map (List(_)) } {
      case (a, b) => for {
        first  <- a continueWith Nil
        second <- b.map(List(_)) continueWith Nil
      } yield first ++ second
    }

  def just[A](v: A) = transform(c => Success((c, v)))
}