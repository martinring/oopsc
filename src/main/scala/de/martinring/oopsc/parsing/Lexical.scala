package de.martinring.oopsc.parsing

import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.CharArrayReader.EofCh

/*
 * This scanner processes OOPS code and produces a lazy stream of tokens. Comments and whitespace are dropped.
 * The scanner distinguishes between keywords, identifiers and numbers as defined in @see Tokens
 * @author Martin Ring
 */
object Lexical extends Scanners with OOPSTokens {
  /* set of reserved words which are parsed as keyword tokens */
  val reserved = Set(
    "CLASS", "IS", "END", "METHOD", "BEGIN", "READ", "NEW",
    "WRITE", "IF", "THEN", "WHILE", "DO", "MOD", "SELF",
    "TRUE", "FALSE", "ELSE", "ELSEIF", "AND", "OR", "NOT",
    "RETURN", "NULL", "EXTENDS", "BASE")

  /* set of delimiter symbols which are parsed as keyword tokens */
  val delimiters = Map(
      "#" -> "NOT EQUAL",
      "(" -> "OPENING PARENTHESES",
      ")" -> "CLOSING PARENTHESES",
      "*" -> "TIMES",
      "+" -> "PLUS",
      "," -> "COMMA",
      "-" -> "MINUS",
      "." -> "ACCESS",
      "/" -> "DIVIDE",
      ":=" -> "ASSIGN",
      ":" -> "TYPE OF",
      ";" -> "SEMICOLON",
      "<=" -> "LESS OR EQUAL",
      "<" -> "LESS",
      "=" -> "EQUAL",
      ">=" -> "GREATER OR EQUAL",
      ">" -> "GREATER") withDefault identity

  /* parses a single token which can be an identifier, a keyword or a number */
  def token: Parser[Token] =
    ( letter ~ (letter|digit*)     ^^ { case first ~ rest => identifierOrKeyword(first :: rest mkString "") }
    | (digit+)                     ^^ { case digits => Number(digits mkString "") }
    | EofCh                        ^^^  EOF
    | delim
    | failure("illegal character") )

  /* parses whitespace and comments and drops them */
  def whitespace: Parser[Any] =
    ( whitespaceChar
    | '{' ~ rep(allExcept(EofCh, '}')) ~ '}'
    | '|' ~ rep(allExcept(EofCh, '\n'))
    | '{' ~ failure("unclosed comment") )*

  /* parses characters A-Z and a-z */
  val letter = elem("letter", ch => ('a' to 'z' contains ch) || ('A' to 'Z' contains ch))

  /* parses characters 0-9 */
  val digit = elem("digit", '0' to '9' contains _)

  /* parses a sequence of characters until one of the specified occurs */
  def allExcept(cs: Char*) = elem("", ch => (cs forall (ch != _)))

  /* parses all characters between 0x00 and 0x20 except EOT (0x04) */
  val whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh)

  /* parses symbols included as a key in @see delimiters */
  val delim: Parser[Token] =
    delimiters.keys.toSeq.sorted.map(d => accept(d.toList) ^^^ Keyword(d))
              .foldRight(failure("no matching delimiter"): Parser[Token])((x,y) => y|x)

  /* decides if a sequence of chars is an identifier or a keyword by looking up if it is contained in the list
  of reserved words */
  def identifierOrKeyword(name: String) =
    if (reserved contains name) Keyword(name) else Identifier(name)
}
