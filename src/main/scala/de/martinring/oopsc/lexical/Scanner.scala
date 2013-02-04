package de.martinring.oopsc.lexical

import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.CharSequenceReader
 
/**
 * This scanner processes OOPS code and produces a lazy stream of tokens. Whitespace is dropped.
 * The scanner distinguishes between keywords, identifiers and numbers as defined in 
 * [[de.martinring.util.oops.lexical.Tokens]]
 * @author Martin Ring
 */
object Scanner extends Scanners with Tokens {
  def apply(c: CharSequence): Scanner = new Scanner(new CharSequenceReader(c))

  /** parses a single token */
  def token: Parser[Token] =
    ( keyword
    | identifier ^^ Identifier
    | integer ^^ Number
    | string
    | failure("illegal character"))

  /** parses whitespace */
  def whitespace: Parser[Any] = (comment | whitespaceChar) *

  /** parses a string token and returns an error */
  def string =
      '\"' ~! (allExcept(EofCh,'\"','\n')*) ~ '\"' ^^ { 
    case _~x~_ => errorToken("Strings are currently unsupported")   
  }
  
  /** parses a comment
   *  comments can be either single line comments starting with '|' and ending with the end of the 
   *  line or multi line comments starting with '{' and ending with '}'. */
  def comment =
    ( '|' ~! (allExcept(EofCh, '\n')*)
    | '{' ~! (allExcept(EofCh, '}')*) ~ '}' )
   
  /** parses an identifier. keyword parser has to be applied bevore to make sure keywords are
   *  not parsed as identifiers */
  def identifier =
    letter ~ (letter|digit*) ^^ { case c~cs => c::cs mkString }
    
  /** parses an integer or a character, which are treated equal internally */
  def integer = 
    ( (digit+) ^^ (_.mkString.toInt)
    | '\'' ~> (allExcept(EofCh,'\'') <~ '\'') ^^ (_.toInt) )
    
  /** parses characters A-Z and a-z */
  def letter = elem("letter", ch => 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z')
  
  /** parses digits 0-9 */
  def digit = elem("digit", ch => '0' <= ch && ch <= '9')
  
  /** parses any character except for the specified 
   * @param cs the characters to fail upon*/  
  def allExcept(cs: Char*) = elem("", ch => (cs forall (ch != _)))
  
  /** parses a single whitespace character */
  def whitespaceChar = elem("space char", _.isWhitespace)
  
  /** parses a keyword as defined in [[de.martinring.oops.lexical.Tokens.keywords]] */
  def keyword: Parser[Token] =
    keywords.toSeq.map { case k => accept(k.chars.toList) ^^^ k }
            .foldRight { failure("no keywords"): Parser[Token] } { (x,y) => y|x }
}