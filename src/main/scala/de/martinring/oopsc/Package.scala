package de.martinring

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/**
 * Global implicits for convenience
 */
package object oopsc {        
  implicit def pimpPositional[T <: Positional](p: T) = new {
    def at(other: Positional) = { p.pos = other.pos; p }
  }
  
  implicit def errors(es: List[Message]) = new {
      def print() { es.foreach ( println(_) ) }
    }
}