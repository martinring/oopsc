package de.martinring.oopsc

import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.output._
import de.martinring.util._
import de.martinring.util.console.ConsoleApp
import java.io._
import scala.io.Source

/**
 * Main class and entry point of the OOPSC Compiler.
 * @author Martin Ring
 */
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
    val showOpt = flag("o", "Show results of optimization")
    val debugMode = flag("d", "Compile debug messages into output")
    val heapSize = namedArgument[Int]("hs", "Set heap size to %s (default is 100)") match {
      case Success(x,_)  => x
      case _ => 100
    }
    val stackSize = namedArgument[Int]("ss", "Set stack size to %s (default is 50)") match {
      case Success(x, _) => x
      case _ => 50
    }
    val sourceFile = argument[File]("source", "the source file", optional = false)
    lazy val source = sourceFile match {     
      case Success(x, _) => Source.fromFile(x, "UTF-8").mkString
      case x =>
        print("[failure] Source File: ")
        x.messages.foreach(println(_))
        sys.exit
    } 
      
      
    val targetFile = argument[String]("target", "the target file", optional = true) match {
      case Success(x, _) => Some(x)
      case x => None
    }    
    val target = targetFile.map(x => () => new PrintWriter(new File(x)))
                           .getOrElse(() => new PrintWriter(new OutputStreamWriter(System.out, "UTF-8")))
  }
  
  if (!arguments.unrecognized.isEmpty) {
    println("[failure] Unrecognized arguments (-h for help): " + arguments.unrecognized.mkString(", "))
  }
  
  if (arguments.showHelp) {
    arguments.printUsage()
    sys.exit()
  }
  
  private val t0 = System.nanoTime
  
  // -------------------------------------------------------------------------------------------------------------------
  //  Lexical analysis
  // -------------------------------------------------------------------------------------------------------------------
  
  private val tokens = lexical.Scanner(arguments.source)
  if (arguments.showSymbols) {
    section("Results of the Lexical Analysis")
    Output(tokens)
  }
  
  // -------------------------------------------------------------------------------------------------------------------
  //  Syntactical analysis
  // -------------------------------------------------------------------------------------------------------------------

  private val p: Program = syntactic.Parser.program(tokens) match {
    case s: syntactic.Parser.Success[_] => s.result
    case f => println(f); sys.exit()
  }
  if (arguments.showSyntax) {
    section("Results of the Syntactical Analysis")
    Output(p)
  }

  
  // -------------------------------------------------------------------------------------------------------------------
  //  Context analysis
  // -------------------------------------------------------------------------------------------------------------------
  
  private val compilation = semantic.ContextAnalysis.analyse(p)() match {
    case Success(p, msgs) => msgs.print; p
    case Errors(p, msgs) => msgs.print; p
    case f => f.messages.print; sys.exit
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
  //  Optimization
  // -------------------------------------------------------------------------------------------------------------------

  private val optimized = semantic.Optimization.optimize(compilation._2)(compilation._1) match {
    case Success(p, msgs) => msgs.print; p
    case Errors(p, msgs) => msgs.print; p
    case f => f.messages.print; sys.exit
  }
  if (arguments.showOpt) {
    section("Results of Optimization")
    Output(optimized._2)
  }
  
  // -------------------------------------------------------------------------------------------------------------------
  //  Code generation
  // -------------------------------------------------------------------------------------------------------------------
  
  val code = synthesis.generate(optimized._2)(optimized._1).run  
  
  // -------------------------------------------------------------------------------------------------------------------
  //  Output
  // -------------------------------------------------------------------------------------------------------------------
  
  private val t = arguments.target()
  code foreach (t.println)    
  
  t.flush()
  
  println("[success] total time: " + ((System.nanoTime - t0) / 1000000) + " ms")  
}