package de.martinring.oopsc

import java.io.File
import io.Source
import ast._
import de.martinring.util._
import de.martinring.util.console.ConsoleApp
import java.io._

object App extends ConsoleApp("OOPSC.jar", "OOPSC Scala Edition, Version 1.7") {
  // -------------------------------------------------------------------------------------------------------------------
  //  Read command line parameters
  // -------------------------------------------------------------------------------------------------------------------
  val arguments = new Arguments {
    import de.martinring.util.Conversions._
    
    val showHelp = flag("h", "Show this help")
    val showSymbols = flag("l", "Show results of lexical analysis")
    val showSyntax = flag("s", "Show results of syntactical analysis")
    val showContext = flag("c", "Show results of context analysis")
    val showVMTs = flag("v", "Show virtual method tables")
    val heapSize = namedArgument[Int]("hs", "Set heap size to %s (default is 100)") match {
      case Success(x,_)  => x
      case _ => 100
    }
    val stackSize = namedArgument[Int]("ss", "Set stack size to %s (default is 50)") match {
      case Success(x, _) => x
      case _ => 50
    }
    val sourceFile = argument[File]("source", "the source file", optional = false) match {
      case Success(x, _) => x
      case x =>
        print("[failure] Source File: ")
        x.messages.foreach(println(_))
        exit
    }
    val source = Source.fromFile(sourceFile, "UTF-8").mkString
    val targetFile = argument[String]("target", "the target file", optional = true) match {
      case Success(x, _) => Some(x)
      case x => None
    }
    val target = targetFile.map(x => new PrintWriter(new File(x)))
                           .getOrElse(new PrintWriter(new OutputStreamWriter(System.out, "UTF-8")))
  }
  
  if (!arguments.unrecognized.isEmpty) {
    println("[failure] Unrecognized arguments (-h for help): " + arguments.unrecognized.mkString(", "))
  }
  
  val t0 = System.nanoTime
  
  // -------------------------------------------------------------------------------------------------------------------
  //  Lexical analysis
  // -------------------------------------------------------------------------------------------------------------------
  
  val tokens = new parsing.Lexical.Scanner(arguments.source)
  if (arguments.showSymbols) {
    section("Results of the Lexical Analysis")
    Output(tokens)
  }
  
  // -------------------------------------------------------------------------------------------------------------------
  //  Syntactical analysis
  // -------------------------------------------------------------------------------------------------------------------

  val p: Program = parsing.program(tokens) match {
    case s: parsing.Success[Program] => s.result
    case f => println(f); sys.exit()
  }
  if (arguments.showSyntax) {
    section("Results of the Syntactical Analysis")
    Output(p)
  }

  
  // -------------------------------------------------------------------------------------------------------------------
  //  Context analysis
  // -------------------------------------------------------------------------------------------------------------------

  val compilation = ContextAnalysis.analyse(p)() match {
    case Success(p, msgs) => msgs.print; p
    case Errors(p, msgs) => msgs.print; p
    case f => f.messages.print; sys.exit()
  }  
  if (arguments.showContext) {
    section("Results of the Context Analysis")
    Output(compilation._2)
  }
  
  if (arguments.showVMTs) {
    section("Virtual Method Tables")
    for ((c,vmt) <- compilation._1.vmts) {
      println("CLASS " + c)
      if (vmt.isEmpty) println("  - Empty -")
      else for ((n,i) <- vmt.zipWithIndex) {
        println("  " + i + " -> " + n)
      }
      println
    }

  }

  // -------------------------------------------------------------------------------------------------------------------
  //  TODO: Optimization
  // -------------------------------------------------------------------------------------------------------------------

  
  // -------------------------------------------------------------------------------------------------------------------
  //  Code generation
  // -------------------------------------------------------------------------------------------------------------------

  //println(compilation._1.declarations.mkString("\n\n"))

  val code = assembler.Code.generate(compilation._2)(compilation._1) match {
    case Success(p, msgs) => msgs.print; p
    case Errors(p, msgs) => msgs.print; p
    case f => f.messages.print; sys.exit
  }
      
  code._2.toString().lines.foreach(arguments.target.println(_))       
  arguments.target.flush()
  
  println("[success] total time: " + ((System.nanoTime - t0) / 1000000) + " ms")
}