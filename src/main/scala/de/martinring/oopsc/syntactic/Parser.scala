package de.martinring.oopsc.syntactic

import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.input.Positional
import de.martinring.oopsc.syntactic._

/**
 * @author Martin Ring
 */
object Parser extends TokenParsers {
  type Tokens = de.martinring.oopsc.lexical.Tokens
  val lexical = de.martinring.oopsc.lexical.Scanner
  import lexical._

  /** Parser for OOPS programs */
  def program: Parser[Program] = phrase( positioned (
      (classdecl+)                                        ^^ { Program(_) } ))

  /** Parser for classes */
  def classdecl: Parser[Class] = positioned {
      CLASS ~> name ~ ( opt(EXTENDS ~> name ) <~ IS) ~
      ( memberdecl * ) <~
      END <~ CLASS ^^
      { case id~base~ms =>
        val attrs = ms.flatten.collect { case a: Variable => a }
        val methods = ms.flatten.collect { case m: Method => m }
        Class(id, attrs, methods, Some(base getOrElse Class.objectClass.name)) at id
      }
  }

  /** Parser for class members. (attributes and methods) */
  def memberdecl: Parser[List[Declaration]] =
    ( attribute <~ ";"
    | method                                              ^^ { List(_) } )

  /** Parser for visibility annotations */
  def visibility: Parser[Visibility.Value] = opt (
      PRIVATE   ^^^ Visibility.Private
    | PROTECTED ^^^ Visibility.Protected
    | PUBLIC    ^^^ Visibility.Public ) ^^ (_ getOrElse Visibility.Public)
    
  /** Parser for method declarations */
  def method: Parser[Method] =
    ( visibility ~ (METHOD ~> name) ~
      opt( "(" ~> rep1sep(variable, ";") <~ ")" ) ~
      opt( ":" ~> name ) <~ IS ) ~
    ( variable <~ ";" *) ~
    ( BEGIN ~> (statement*)) <~
      END <~ METHOD                                   ^^
    { case vis~id~params~typed~vars~body =>
        Method(
          id,
          params.map(_.flatten) getOrElse Nil,
          vars.flatten,
          body,
          typed getOrElse Class.voidType.name,
          None,
          vis) at id }

  /** Parser for attributes */
  def attribute: Parser[List[Variable]] =
      visibility ~ rep1sep(name, "," ) ~ (":" ~> name)    ^^ { case v~ids~t => ids map (id => Variable(id, t, None, true, v) at id) }

  /** Parser for variables */
  def variable: Parser[List[Variable]] =
      rep1sep(name, "," ) ~ (":" ~> name)                 ^^ { case ids~t => ids map (id => Variable(id, t, None, false) at id) }

  /** Parser for `READ`, `WRITE`, `IF`, `WHILE`, `RETURN`, `CALL` and `ASSIGN` statements */
  def statement: Parser[Statement] = positioned(
      READ  ~> memberaccess <~ ";"                        ^^ { Read(_) }
    | WRITE ~> expr <~ ";"                                ^^ { Write(_) }
    | IF ~ (disjunction <~
      THEN) ~ (statement*) ~
       opt(elseIf)  <~
      END <~ IF                                           ^^ { case op~cond~body~elseBody => If(cond, body, elseBody getOrElse Nil) at op }
    | WHILE ~> (disjunction <~
      DO ) ~ (statement*) <~
      END <~ WHILE                                        ^^ { case cond~body => While(cond, body) }
    | RETURN ~> opt(disjunction) <~ ";"                   ^^ { Return(_) }
    | memberaccess <~ ";"                                 ^^ { Call(_) }
    | memberaccess ~ ":=" ~ disjunction <~ ";"            ^^ { case left~op~right => Assign(left, right) at op }
    | failure ("illegal start of statement"))

  private def elseIf: Parser[List[Statement]] =
    ( ELSE ~> (statement*)
    | ELSEIF ~ disjunction ~ ( THEN ~>
      (statement*) ~
      opt(elseIf))                                        ^^ { case op~cond~(body~elseBody) => List(If(cond,body,elseBody getOrElse Nil) at op) } )

  /** combinator for left associative binary operators */
  def binopl(expr: Parser[Expression], ops: Parser[Keyword with Positional]): Parser[Expression] = 
      expr ~ ( ops ~ expr * ) ^^
    { case l ~ rs => rs.foldLeft(l) { case (l,op~r) => Binary(op.chars,l,r) at op } }
  
  /** combinator for right associative binary operators */
  def binopr(expr: Parser[Expression], ops: Parser[Keyword with Positional]): Parser[Expression] =  
   ( expr ~ ops ~ binopr(expr,ops) ^^ { case l ~ op ~ r => Binary(op.chars,l,r) at op }
   | expr )
  
  def disjunction: Parser[Expression] = binopr(conjunction, OR ~> ELSE | OR)  
  def conjunction: Parser[Expression] = binopr(relation, AND ~> THEN | AND)
  def relation:    Parser[Expression] = binopl(expr,"="|"#"|"<"|">"|"<="|">=")  
  def expr:        Parser[Expression] = binopl(term,"+"|"-")
  def term:        Parser[Expression] = binopl(factor,"*"|"/"| MOD)
  
  def factor: Parser[Expression] =
    ( ("-"| NOT) ~ factor                                ^^ { case op~f => Unary(op.chars, f) at op }
    | memberaccess )

  def memberaccess: Parser[Expression] = 
      literal ~ ("." ~ varorcall *) ^^ 
    { case lit~vs => vs.foldLeft(lit){ case (l,(o~r)) => Access(l, r) at o }}
  
  def literal: Parser[Expression] = positioned (
      number                                              ^^ { Literal.Int(_) }
    | FALSE                                               ^^^{ Literal.False }
    | TRUE                                                ^^^{ Literal.True }
    | NULL                                                ^^^{ Literal.Null }
    | SELF                                                ^^^{ VarOrCall(new RelativeName("SELF")) }
    | BASE                                                ^^^{ VarOrCall(new RelativeName("BASE")) }
    | NEW ~> name                                         ^^ { x => New(x) at x }
    | "(" ~> disjunction <~ ")"
    | varorcall 
    | failure("expression expected"))

  def varorcall: Parser[VarOrCall] = positioned (
    name ~ opt( "(" ~> rep1sep(disjunction, ",") <~")" )  ^^ { case name~params => VarOrCall(name, params getOrElse Nil) } )

  def name =  positioned(
    elem("identifier", _.isInstanceOf[Identifier])        ^^ { c => RelativeName(c.chars) } )

  def number     =
    elem("number", _.isInstanceOf[Number])                ^^ ( _.chars.toInt )
  
  /** Convenience implicit to convert [[Keyword]] to a [[Parser[Keyword]]] **/
  implicit def keyword(k: Keyword): Parser[Keyword with Positional] = 
    positioned( elem(k) ^^^ new Keyword(k.chars) with Positional { } )
        
    /** Convenience implicit to convert [[String]] literals to Parsers for the
      corresponding delimiter as defined in [[keywords]] **/
  implicit def delimiter(d: String): Parser[Keyword with Positional] = keyword(keywords.find(_.chars == d).get)
  implicit def delimiter(d: Char):   Parser[Keyword with Positional] = keyword(keywords.find(_.chars == d.toString).get)  
}