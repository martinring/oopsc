package de.martinring

import de.martinring.oopsc.output._
import java.io.File
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional


/**
 * The OOPS compiler is for compiling OOPS programs into OOPSVM assembler 
 * instructions.
 */
package object oopsc {

  implicit def pimpPositional[T <: Positional](p: T) = new {
    def at(other: Positional) = { p.pos = other.pos; p }
  }
  
  /**
   * Provides additional functions on [[java.lang.String]].
   */
  class PimpedString(s: String) {
    /** Decorates a Message with position information and Wraps it in an [[de.martinring.oopsc.Error]] */
    def at (p: Positional) = Error(p.pos, s)

    /** Repeats the String n times */
    def repeat(n: Int) = Iterator.fill(n)(s).mkString

    /** Converts the String to an optional Int which is 'None' if the parsing failed
     *  or 'Some(x)' if the String parses to 'x' */
    def toIntOption: Option[Int] = {
      try Some(s.toInt)
      catch { case _ => None }
    }

    /** Indents a String by n spaces on each line */
    def indent(n: Int) = s.split("\n").map(" ".repeat(n) + _).mkString("\n")    
  }

  implicit def pimpString(s: String): PimpedString = new PimpedString(s)

  implicit def pimpList[T](list: List[T]) = new {    
    def prefixes = for (i <- 0 to list.size)
      yield list.take(i)
  }

  implicit def errors(es: List[Message]) = new {
      def print() {
        es.foreach ( println(_) )
        if (es.exists(_.isInstanceOf[Error])) sys.exit
      }
    }
}