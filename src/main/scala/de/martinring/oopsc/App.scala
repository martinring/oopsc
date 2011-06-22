package de.martinring.oopsc

import java.io.File
import io.Source
import ast._
import parsing._
import Failable._
import de.martinring.util._
import de.martinring.util.Console._

object App extends App {
  // -------------------------------------------------------------------------------------------------------------------
  //  Read command line parameters
  // -------------------------------------------------------------------------------------------------------------------

  val arguments = new Arguments(args,
  "OOPSC Scala Edition - Version 1.0",
  "",
  "Usage: java -jar oopsc.jar [-h] [-c] [-l] [-i] [-s] [-hs <n>] [-ss <n>] source [target]",
  "",
  "    -h       Show help",
  "    -l       Show results of lexical analysis",
  "    -s       Show results of syntactical analysis",
  "    -c       Show results of context analysis",
  "    -i       Show identifiers",
  "    -hs <n>  Set heap size to <n> (default is 50)",
  "    -ss <n>  Set stack size to <n> (default is 250)")
  
  import arguments._

  if (flag("h")) printUsage()

  val showContext = flag("c")
  val showSymbols = flag("l")
  val showIdentifiers = flag("i")
  val showSyntax = flag("s")
  val heapSize = option("hs").flatMap(_.toIntOption) getOrElse 50
  val stackSize = option("ss").flatMap(_.toIntOption) getOrElse 250
  val inFile = plain.map(_.open) getOrElse fail("Failure: No source file specified")
  val outFile = plain.map(_.open)

  arguments.finish()

  // Check source and target files
  if (!inFile.exists) fail("Failure: Source File '%s' does not exist!\n".format(inFile.getName))
  outFile.map{f =>
    if (!f.exists() && !f.createNewFile() || !f.canWrite)
      fail("Failure: Can not write to '%s'!\n".format(f.getAbsolutePath))}

  // Create reader for source file
  val source = Source.fromFile(inFile, "UTF-8").mkString

  // -------------------------------------------------------------------------------------------------------------------
  //  Lexical analysis
  // -------------------------------------------------------------------------------------------------------------------

  val tokens = new Lexical.Scanner(source)
  if (showSymbols) {
    section("Results of the Lexical Analysis")    
    Output(tokens)
  }

  // -------------------------------------------------------------------------------------------------------------------
  //  Syntactical analysis
  // -------------------------------------------------------------------------------------------------------------------

  val p: Program = program(tokens) match {
    case s: parsing.Success[Program] => s.result
    case f => println(f); sys.exit()
  }
  if (showSyntax) {
    section("Results of the Syntactical Analysis")
    Output(p)
  }

  // -------------------------------------------------------------------------------------------------------------------
  //  Context analysis
  // -------------------------------------------------------------------------------------------------------------------

  val compilation: Program = ContextAnalysis.program(p)() match {
    case Success((c,p), msgs) => msgs.print; p
    case Errors((c,p), msgs) => msgs.print; p
    case f => f.messages.print; sys.exit()
  }
  if (showContext) {
    section("Results of the Context Analysis")
    Output(compilation)
  }
  
    
  // -------------------------------------------------------------------------------------------------------------------
  //  TODO: Optimization
  // -------------------------------------------------------------------------------------------------------------------

    
  // -------------------------------------------------------------------------------------------------------------------
  //  TODO: Code generation
  // -------------------------------------------------------------------------------------------------------------------

  println(assembler.Code.generate(compilation)().get._2)
}