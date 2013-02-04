package de.martinring.util

object Failable {
  implicit class FailableOption[T](opt: Option[T]) {
    def orFail[E](error: E): Failable[T,E] = opt.map(Success(_,Nil)).getOrElse(Failure(error))
  }    
}

sealed abstract class Failable[+A,+E] {
  def map[B](f: A => B): Failable[B,E] = this match {
    case Success(res,es) => Success(f(res),es)
    case Errors(res,es)  => Errors(f(res),es)
    case Failure(es)     => Failure(es)
  }  
  
  def flatMap[B,E2 >: E](f: A => Failable[B,E2]): Failable[B,E2] = this match {
    case Success(res,es1) => f(res) match {
      case Success(res,es2) => Success(res,es1 ++ es2)
      case Errors(res,es2)  => Errors(res,es1 ++ es2)
      case Failure(es2)     => Failure(es1 ++ es2)
    }
    case Errors(res,es1) => f(res) match {
      case Success(res,es2) => Errors(res,es1 ++ es2)
      case Errors(res,es2)  => Errors(res,es1 ++ es2)
      case Failure(es2)     => Failure(es1 ++ es2)
    }
    case Failure(es) => Failure(es)
  }
  
  def messages: List[E]
}

case class Success[A,E](result: A, messages: List[E] = Nil) extends Failable[A,E]
case class Errors[A,E](result: A, messages: List[E]) extends Failable[A,E]
object Failure {
  def apply[E](error: E) = new Failure(error)
}
case class Failure[E](messages: List[E]) extends Failable[Nothing,E] {
  def this(error: E) = this(List(error))
}