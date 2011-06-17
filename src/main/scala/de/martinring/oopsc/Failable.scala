package de.martinring.oopsc

import scala.util.parsing.input.Position

/*
 * Special error monad, that can collect errors while the result is still usable (to continue compilation after non
 * fatal errors) or fail if the result is unusable.
 * @author Martin Ring
 */
sealed abstract class Failable[+A] {
  def get: A
  def messages: List[Message]

  def map[B](f: A => B): Failable[B] = this match {
    case Success(a, es) => Success(f(a), es)
    case Errors(a, es) => Errors(f(a), es)
    case fail: Failure => fail
  }

  def flatMap[B](f: A => Failable[B]): Failable[B] = this match {
    case Success(a, ms) => f(a) match {
        case Success(b, ms2) => Success(b, ms ++ ms2)
        case Errors(b, ms2) => Errors(b, ms ++ ms2)
        case Failure(ms2) => Failure(ms ++ ms2)
    }
    case Errors(a, ms) => f(a) match {
        case Success(b, ms2) => Errors(b, ms ++ ms2)
        case Errors(b, ms2) => Errors(b, ms ++ ms2)
        case Failure(ms2) => Failure(ms ++ ms2)
    }
    case fail: Failure => fail
  }
}

case class Success[+A](get: A, messages: List[Message] = Nil) extends Failable[A]    
case class Errors[+A](get: A, messages: List[Message]) extends Failable[A]
case class Failure(messages: List[Message]) extends Failable[Nothing] {
  def get = throw new NoSuchElementException("Failure.get")
}

object Failable {
  def fail(msg: Message) = Failure(List(msg))
  
  def merge[T](list: List[Failable[T]]) = list.foldRight(Success(Nil): Failable[List[T]]){
    case (a, b) if a.isInstanceOf[Failure] || b.isInstanceOf[Failure] => Failure(a.messages ++ b.messages)
    case (a, Errors(tail,ms))                                         => Errors(a.get :: tail, a.messages ++ ms)
    case (Errors(head, ms), b)                                        => Errors(head :: b.get, ms ++ b.messages)
    case (Success(head, ms), Success(tail,ms2))                       => Success(head :: tail, ms ++ ms2)
  }
  
  implicit def pimpOption[T](opt: Option[T]) = new {
    def orFail(msg: Message): Failable[T] = opt match {
      case Some(t) => Success(t)
      case None    => fail(msg)
    }
  }
}