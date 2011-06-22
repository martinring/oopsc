package de.martinring.oopsc

import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.Positional
import de.martinring.oopsc.ast._

/**
 * @author Martin Ring
 */
package object parsing extends TokenParsers {
  type Tokens = de.martinring.oopsc.parsing.OOPSTokens
  val lexical = de.martinring.oopsc.parsing.Lexical
  import lexical._

  def program: Parser[Program] = positioned (
      classdecl                                           ^^ { Program(_) } )

  def classdecl: Parser[Class] = positioned (
      "CLASS" ~> name ~ "IS" ~
      ( memberdecl * ) <~
      "END" <~ "CLASS"                                    ^^ { case id~_~ms => Class(id.name, ms.flatten) at id })

  def memberdecl: Parser[List[Member]] =
    ( attribute <~ ";"
    | method                                              ^^ { List(_) } )

  def method: Parser[Method] =
      "METHOD" ~> name ~ "IS" ~
      ( variable <~ ";" *) ~
      "BEGIN" ~ (statement*) <~
      "END" <~ "METHOD"                                   ^^ { case id~_~vars~_~body => Method(id.name, vars.flatten, body) at id }

  def attribute: Parser[List[Attribute]] =
      rep1sep( name, "," ) ~ ":" ~ name                   ^^ { case ids~_~t => ids map (id => Attribute(id.name, t) at id) }
  
  def variable: Parser[List[Variable]] =
      rep1sep( name, "," ) ~ ":" ~ name                   ^^ { case ids~_~t => ids map (id => Variable(id.name, t) at id) }

  def statement: Parser[Statement] = positioned(
      "READ" ~> memberaccess <~ ";"                       ^^ { Read(_) }
    | "WRITE" ~> expr <~ ";"                              ^^ { Write(_) }
    | "IF" ~> relation ~
      "THEN" ~ (statement*) <~
      "END" <~ "IF"                                       ^^ { case cond~_~body => If(cond, body) }
    | "WHILE" ~> relation ~
      "DO" ~ (statement*) <~
      "END" <~ "WHILE"                                    ^^ { case cond~_~body => While(cond, body) }
    | memberaccess <~ ";"                                 ^^ { Call(_) }
    | memberaccess ~ ":=" ~ expr <~ ";"                   ^^ { case left~op~right => Assign(left, right) at op }
    | failure ("illegal start of statement"))


  def relation: Parser[Expression] = positioned(
      expr ~ (("="|"#"|"<"|">"|"<="|">=") ~ expr *)       ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }})

  def expr: Parser[Expression] = positioned(
      term ~ (("+"|"-") ~ term *)                         ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }})

  def term: Parser[Expression] = positioned(
      factor ~ (("*"|"/"|"MOD") ~ factor *)               ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }})

  def factor: Parser[Expression] = positioned (
      "-" ~ factor                                        ^^ { case op~f => Unary(op.chars, f) at op }
    | memberaccess )

  def memberaccess: Parser[Expression] = positioned (
      literal ~ ("." ~ name *)                            ^^ { case lit~vs => vs.foldLeft(lit){ case (l,(o~r)) => Access(l, r) at o }})

  def literal: Parser[Expression] = positioned (
      number                                              ^^ { x => Literal(x, Class.intType.name) }
    | "NULL"                                              ^^^{ Literal(0, Class.nullType.name) }
    | "SELF"                                              ^^^{ Name("SELF") }
    | "NEW" ~> name                                       ^^ { x => New(x) at x }
    | "(" ~> expr <~ ")"
    | name )

  def name: Parser[Name] = positioned (
    elem("identifier", _.isInstanceOf[Identifier])        ^^ { case i@Identifier(chars) => Name(chars) } )

  def number     =
    elem("number", _.isInstanceOf[Number])                ^^ ( _.chars.toInt )

  implicit def keyword(k: String): Parser[Keyword with Positional] = positioned( accept(Keyword(k)) ^^^ new Keyword(k) with Positional )
}