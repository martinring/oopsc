package de.martinring.oopsc.parsing
import scala.util.parsing.input.Positional

/*
 * Defines the tokens that are distinguished in the OOPS language.
 * @author Martin Ring
 */

trait OOPSTokens extends scala.util.parsing.combinator.token.Tokens {
  /* Represents a keyword (Reserved word or delimiter) */
  case class Keyword(chars: String) extends Token  {
    override def toString = Lexical.delimiters(chars)
  }
  /* Represents a number literal */
  case class Number(chars: String) extends Token {
    require(!chars.exists(x => !('0' to '9').contains(x)))
    val value = chars.toInt
    override def toString = "NUMBER " + chars
  }
  /* Represents an identifier */
  case class Identifier(chars: String) extends Token with Positional {
    override def toString = "IDENTIFIER " + chars
  }
}