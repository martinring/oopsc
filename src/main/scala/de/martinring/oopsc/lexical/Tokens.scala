package de.martinring.oopsc.lexical
import scala.collection.immutable.SortedSet

/** 
 * Describes all token types of the OOPS language 
 * @author Martin Ring 
 */
trait Tokens extends scala.util.parsing.combinator.token.Tokens {
  /** Set of Keywords */
  val keywords = SortedSet(
    AND,BASE,BEGIN,CLASS,DO,ELSE,ELSEIF,END,EXTENDS,FALSE,IF,IS,ISA,METHOD,MOD,
    NEW,NOT,NULL,OR,PRIVATE,PROTECTED,PUBLIC,READ,RETURN,SELF,THEN,TRUE,WHILE,
    WRITE,ACCESS,ASSIGN,CLOSING_PARENTHESES,COMMA,DIVIDE,EQUAL,GREATER,
    GREATER_OR_EQUAL,LESS,LESS_OR_EQUAL,MINUS,NOT_EQUAL,OPENING_PARENTHESES,
    PLUS,SEMICOLON,TIMES,TYPE_OF)(Ordering.by(_.chars))

  /** For improved performance, keywords are represented by fly-weight-tokens
   *  That means, there is only one instance of each keyword token which can be
   *  matched by address rather then structure. All these tokens derive from
   *  this base class. */
  abstract class Keyword(val chars: String) extends Token

  case object AND       extends Keyword("AND")
  case object BASE      extends Keyword("BASE")
  case object BEGIN     extends Keyword("BEGIN")
  case object CLASS     extends Keyword("CLASS")
  case object DO        extends Keyword("DO")
  case object ELSE      extends Keyword("ELSE")
  case object ELSEIF    extends Keyword("ELSEIF")
  case object END       extends Keyword("END")
  case object EXTENDS   extends Keyword("EXTENDS")
  case object FALSE     extends Keyword("FALSE")
  case object IF        extends Keyword("IF")
  case object IS        extends Keyword("IS")
  case object ISA       extends Keyword("ISA")
  case object METHOD    extends Keyword("METHOD")
  case object MOD       extends Keyword("MOD")
  case object NEW       extends Keyword("NEW")
  case object NOT       extends Keyword("NOT")
  case object NULL      extends Keyword("NULL")
  case object OR        extends Keyword("OR")
  case object PRIVATE   extends Keyword("PRIVATE")
  case object PROTECTED extends Keyword("PROTECTED")
  case object PUBLIC    extends Keyword("PUBLIC")
  case object READ      extends Keyword("READ")
  case object RETURN    extends Keyword("RETURN")
  case object SELF      extends Keyword("SELF")
  case object THEN      extends Keyword("THEN")
  case object TRUE      extends Keyword("TRUE")
  case object WHILE     extends Keyword("WHILE")
  case object WRITE     extends Keyword("WRITE")

  case object ACCESS              extends Keyword(".")
  case object ASSIGN              extends Keyword(":=")
  case object CLOSING_PARENTHESES extends Keyword(")")
  case object COMMA               extends Keyword(",")
  case object DIVIDE              extends Keyword("/")
  case object EQUAL               extends Keyword("=")
  case object GREATER             extends Keyword(">")
  case object GREATER_OR_EQUAL    extends Keyword(">=")
  case object LESS                extends Keyword("<")
  case object LESS_OR_EQUAL       extends Keyword("<=")
  case object MINUS               extends Keyword("-")
  case object NOT_EQUAL           extends Keyword("#")
  case object OPENING_PARENTHESES extends Keyword("(")
  case object PLUS                extends Keyword("+")
  case object SEMICOLON           extends Keyword(";")
  case object TIMES               extends Keyword("*")
  case object TYPE_OF             extends Keyword(":")

  /** Represents a number literal */
  case class Number(value: Int) extends Token { lazy val chars = value.toString }

  /** Represents an identifier */
  case class Identifier(chars: String) extends Token

  /** Represents a comment (could be dropped but is important for front end) */
  case class Comment(chars: String) extends Token
}