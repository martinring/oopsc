package de.martinring

import de.martinring.oopsc.syntactic.Declaration
import java.io.File
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/**
 * Global implicits for convenience
 */
package object oopsc {

  implicit def pimpPositional[T <: Positional](p: T) = new {
    def at(other: Positional) = { p.pos = other.pos; p }
  }

  class PimpedString(s: String) {
    def at (p: Positional) = Error(p.pos, s)

    def repeat(n: Int) = Iterator.fill(n)(s).mkString

    def fill(params: (String, Any)*) =
      params.foldLeft(s) { case (s, (name, value)) => s.replaceAll("{" + name + "}", value.toString) }

    def toIntOption: Option[Int] = {
      try Some(s.toInt)
      catch { case _ => None }
    }

    def indent(by: Int) = s.split("\n").map(" ".repeat(by) + _).mkString("\n")

    def open: File = new File(s)
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