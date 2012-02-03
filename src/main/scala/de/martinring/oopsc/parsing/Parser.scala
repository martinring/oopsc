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

  def program: Parser[Program] = phrase( positioned (
      (classdecl+)                                        ^^ { Program(_) } ))

  def classdecl: Parser[Class] = positioned {
      "CLASS" ~> name ~ ( opt("EXTENDS" ~> name ) <~ "IS") ~
      ( memberdecl * ) <~
      "END" <~ "CLASS" ^^
      { case id~base~ms =>
        val attrs = ms.flatten.collect { case a: Variable => a }
        val methods = ms.flatten.collect { case m: Method => m }
        Class(id, attrs, methods, Some(base getOrElse Class.objectClass.name)) at id
      }
  }

  def memberdecl: Parser[List[Declaration]] =
    ( attribute <~ ";"
    | method                                              ^^ { List(_) } )

  def method: Parser[Method] =
    ( "METHOD" ~> name ~
      opt( "(" ~> rep1sep(variable, ";") <~ ")" ) ~
      opt( ":" ~> name ) <~ "IS" ) ~
    ( variable <~ ";" *) ~
    ( "BEGIN" ~> (statement*)) <~
      "END" <~ "METHOD"                                   ^^
    { case id~params~typed~vars~body =>
        Method(
          id,
          params.map(_.flatten) getOrElse Nil,
          vars.flatten,
          body,
          typed getOrElse Class.voidType.name,
          None) at id }

  def attribute: Parser[List[Variable]] =
      rep1sep(name, "," ) ~ (":" ~> name)                 ^^ { case ids~t => ids map (id => Variable(id, t, None, true) at id) }

  def variable: Parser[List[Variable]] =
      rep1sep(name, "," ) ~ (":" ~> name)                 ^^ { case ids~t => ids map (id => Variable(id, t, None, false) at id) }

  def statement: Parser[Statement] = positioned(
      "READ" ~> memberaccess <~ ";"                       ^^ { Read(_) }
    | "WRITE" ~> expr <~ ";"                              ^^ { Write(_) }
    | "IF" ~> (disjunction <~
      "THEN") ~ (statement*) ~
       opt(elseIf)  <~
      "END" <~ "IF"                                       ^^ { case cond~body~elseBody => If(cond, body, elseBody getOrElse Nil) }
    | "WHILE" ~> (disjunction <~
      "DO" ) ~ (statement*) <~
      "END" <~ "WHILE"                                    ^^ { case cond~body => While(cond, body) }
    | "RETURN" ~> opt(disjunction) <~ ";"                 ^^ { Return(_) }
    | memberaccess <~ ";"                                 ^^ { Call(_) }
    | memberaccess ~ ":=" ~ disjunction <~ ";"            ^^ { case left~op~right => Assign(left, right) at op }
    | failure ("illegal start of statement"))

  def elseIf: Parser[List[Statement]] =
    ( "ELSE" ~> (statement+)
    | "ELSEIF" ~> disjunction ~ ( "THEN" ~>
      (statement*) ~
      opt(elseIf))                                        ^^ { case cond~(body~elseBody) => List(If(cond,body,elseBody getOrElse Nil)) } )

  def disjunction: Parser[Expression] = positioned(
      conjunction ~ ("OR" ~ conjunction *)                ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }}
    | failure("expression expected"))

  def conjunction: Parser[Expression] = positioned(
      relation ~ ("AND" ~ relation *)                     ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }})

  def relation: Parser[Expression] = positioned(
      expr ~ (("="|"#"|"<"|">"|"<="|">=") ~ expr *)       ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }})

  def expr: Parser[Expression] = positioned(
      term ~ (("+"|"-") ~ term *)                         ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }})

  def term: Parser[Expression] = positioned(
      factor ~ (("*"|"/"|"MOD") ~ factor *)               ^^ { case l~rs => rs.foldLeft(l){ case (l,(o~r)) => Binary(o.chars, l, r) at o }})

  def factor: Parser[Expression] = positioned (
      ("-"|"NOT") ~ factor                                ^^ { case op~f => Unary(op.chars, f) at op }
    | memberaccess )

  def memberaccess: Parser[Expression] = positioned (
      literal ~ ("." ~ varorcall *)                       ^^ { case lit~vs => vs.foldLeft(lit){ case (l,(o~r)) => Access(l, r) at o }})

  def literal: Parser[Expression] = positioned (
      number                                              ^^ { x => Literal(x, Class.intType.name) }
    | "FALSE"                                             ^^^{ Literal(0, Class.boolType.name) }
    | "TRUE"                                              ^^^{ Literal(1, Class.boolType.name) }
    | "NULL"                                              ^^^{ Literal(0, Class.nullType.name) }
    | "SELF"                                              ^^^{ VarOrCall(new RelativeName("SELF")) }
    | "BASE"                                              ^^^{ VarOrCall(new RelativeName("BASE")) }
    | "NEW" ~> name                                       ^^ { x => New(x) at x }
    | "(" ~> disjunction <~ ")"
    | varorcall )

  def varorcall: Parser[VarOrCall] = positioned (
    name ~ opt( "(" ~> rep1sep(disjunction, ",") <~")" )  ^^ { case name~params => VarOrCall(name, params getOrElse Nil) } )

  def name =  positioned(
    elem("identifier", _.isInstanceOf[Identifier])        ^^ { c => RelativeName(c.chars) } )

  def number     =
    elem("number", _.isInstanceOf[Number])                ^^ ( _.chars.toInt )

  implicit def keyword(k: String): Parser[Keyword with Positional] = positioned( accept(Keyword(k)) ^^^ new Keyword(k) with Positional )
}