package de.martinring.oopsc

import de.martinring.oopsc.ast._

object  Code {
  val lf = "\n"
  def lines(ls: String*) = ls.mkString(lf)

  def generate(element: Element): String = element match {
    case Program(main) => lines(
      // Start-Code: Register initialisieren
      "MRI R1, 1 ; R1 ist immer 1",
      "MRI R2, _stack ; R2 zeigt auf den Stapel",
      "MRI R4, _heap ; R4 zeigt auf die nächste freie Stelle auf dem Heap",
      generate(main),
      "MRI R0, _end ; Program beenden"
    )

  }
}