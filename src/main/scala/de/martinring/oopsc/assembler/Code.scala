package de.martinring.oopsc.assembler

import de.martinring.oopsc.ast._
import de.martinring.oopsc.Assembler._
import de.martinring.oopsc._
import de.martinring.oopsc.Transform._


object  Code {
  val lf = "\n"
  def lines(ls: String*) = ls.mkString(lf)

  val stack = "_stack"
  val heap = "_heap"
  val end = "_end"

  def generate(element: Element): Transform[String] = element match {
    case Program(main) => for {
      main <- generate(main)
    } yield lines(
      // Start-Code: Register initialisieren
      R(1) := 1, // R1 is immer 1
      R(2) := stack, // R2 zeigt auf den Stapel",
      R(4) := heap, // R4 zeigt auf die nächste freie Stelle auf dem Heap",
      main,
      R(0) := end) // Program beenden"

    case c: Class => for {
      _       <- enter(c)
      methods <- merge(c.methods.map(generate))
      _       <- leave
    } yield methods.mkString("\n")

    case m: Method => for {
      _    <- enter(m)
      body <- merge(m.body map generate)
      _    <- leave
    } yield body.mkString("\n")

    case x => success(x.toString)
  }
}