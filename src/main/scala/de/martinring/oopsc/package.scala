package de.martinring

import de.martinring.oopsc.output._
import java.io.File
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/**
 * The OOPS compiler can be used for compiling OOPS programs into OOPSVM assembler 
 * instructions.
 */
package object oopsc {
  implicit class PimpedPositional[T <: Positional](p: T) {
    def at(other: Positional) = { p.pos = other.pos; p }
  }
  
  /**
   * Provides additional functions on [[java.lang.String]].
   */
  implicit class PimpedString(s: String) {
    /** Decorates a Message with position information and Wraps it in an [[de.martinring.oopsc.Error]] */
    def at (p: Positional) = Error(p.pos, s)

    /** Repeats the String n times */
    def repeat(n: Int) = Iterator.fill(n)(s).mkString

    /** Converts the String to an optional Int which is 'None' if the parsing failed
     *  or 'Some(x)' if the String parses to 'x' */
    def toIntOption: Option[Int] = scala.util.Try(s.toInt).toOption      

    /** Indents a String by n spaces on each line */
    def indent(n: Int) = s.split("\n").map(PimpedString(" ").repeat(n) + _).mkString("\n")    
  }

  implicit class PimpedList[T](list: List[T]) {    
    def prefixes = for (i <- 0 to list.size)
      yield list.take(i)
  }

  implicit class PimpedMessageList(es: List[Message]) {
      def print() {
        es.filter(!_.internal).foreach ( println(_) )
        if (es.exists(_.isInstanceOf[Error])) sys.exit
      }
    }
}