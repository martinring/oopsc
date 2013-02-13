package de.martinring.oopsc

import org.specs2.mutable._
import de.martinring.util._
import de.martinring.oopsc.output._
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.lexical._
import scala.util.parsing.input.Position
import scala.io.Source
import de.martinring.oopsc.semantic.ContextAnalysis
import de.martinring.oopsc.semantic.Optimization
import de.martinring.oopsc.semantic.Context
import java.io._
import scala.sys.process._

class AppTest extends Specification {
  def compile(file: String): Failable[(Program,Context),Message] = {
    val source = Source.fromFile(f"$file.oops").mkString
    val lex = Scanner(source)    
    for {
      prog <- Parser.program(lex) match {
	    case Parser.Success(prog,_) => Success(prog, Nil)
	    case e: Parser.NoSuccess  => Failure(Error(e.next.pos,e.msg)) 
      }
      (con,prog) <- ContextAnalysis.analyse(prog)()
      (con,prog) <- Optimization.optimize(prog)(con)
    } yield (prog,con)
  }
  
  //def run(file: String)(input: String = ""): String = {
  //  App.main(Array(f"$file.oops",f"$file.asm"))
  //  def write(out: OutputStream) = {
  //    out.close()
  //  }
  //  def read(in: InputStream) = {
  //    in.close()
  //  }
  //  (f"java -jar OOPSVM.jar $file.asm") ! new ProcessIO(write,read,read)
  //  "hallo"
  //}
  
  def convertStreamToString(is: InputStream): String = {
	val s = new java.util.Scanner(is).useDelimiter("\\A")
	if (s.hasNext()) s.next() else ""	
  }
  
  def checkOutput(file: String, inOuts: (String,String)*) {
    f"$file.oops" should {
      val comp = compile(file)
      "compile without errors" in {
        comp.messages.filter(_.isError) must haveSize(0)
      }
      App.main(Array(f"$file.oops",f"$file.asm"))
      for ((in,out) <- inOuts) {                
        if (in == "") {
          (f"return '$out' when executed") in {
            val s = (f"java -jar OOPSVM.jar $file.asm" !!)
            s.linesIterator.mkString("\n") must be_==(out)
          }
        } 
        else {
          (f"return '$out' on input '$in'") in {
            var res = ""
            val io = new ProcessIO(
                stdin  => {
                  stdin.write(in.getBytes)
                  stdin.close()
                }, 
                stdout => {
                  res = convertStreamToString(stdout)
                  stdout.close() 
                },
                stderr => stderr.close())
            val proc = f"java -jar OOPSVM.jar $file.asm".run(io)
            proc.exitValue() must be_==(0)
            res must be_==(out)
          }
        }
      }
      
    }
  }

  def checkErrors(file: String, errors: String*) = {
    f"$file.oops" should {
      val comp = compile(file)
      "not compile without errors" ! (!comp.messages.filter(_.isError).isEmpty)
      errors.foreach { error =>
        (f"contain an error about '$error'") in {
          comp.messages.exists(_.message.toLowerCase.containsSlice(error.toLowerCase))
        }
      }
    }
  }

  checkOutput("examples/binding", "" -> "ABBA")
  checkOutput("examples/boolean1", "abc" -> "OK", "cba" -> "", "cda" -> "OK")
  checkErrors("examples/boolean2", "type mismatch")
  checkErrors("examples/boolean3", "type mismatch")
  checkOutput("examples/classes1", "" -> "OK")
  checkErrors("examples/classes2", "already defined")
  checkOutput("examples/code", "Hallo" -> ("Hallo".toList.map(_.toInt).mkString(" ") + " "),
                               "Garbage Collector" -> ("Garbage Collector".toList.map(_.toInt).mkString(" ") + " "))
  checkOutput("examples/echo", "Hallo Welt" -> "Hallo Welt")
  checkOutput("examples/else1", "" -> "ABCDEFGH")
  checkErrors("examples/else2", "`IF' expected but METHOD found")
  checkErrors("examples/else3", "`END' expected but ELSE found")
  checkErrors("examples/else4", "`THEN' expected but WRITE found")
  checkOutput("examples/extends1", "" -> "ABBAA")
  checkErrors("examples/extends2", "cyclic inheritance")
  checkErrors("examples/extends3", "no main class defined")
  checkErrors("examples/extends4", "no main class defined")
  checkErrors("examples/extends5", "missing return in method a.a", "missing return in method b.a", "no main class defined")
  checkErrors("examples/extends6", "missing return in method a.a")
  checkErrors("examples/extends7", "object is already defined")
  checkErrors("examples/extends8", "l-value expected")
  checkOutput("examples/parameters1", "" -> "ABCDEDCBA")
  checkErrors("examples/parameters2", "wrong number of arguments")
  checkErrors("examples/parameters3", "wrong number of arguments")
  checkErrors("examples/parameters4", "type mismatch")
  checkErrors("examples/parameters5", "b is already defined")
  checkErrors("examples/parameters6", "main method must not take parameters")
  checkOutput("examples/return1", "" -> "ABCD")
  checkErrors("examples/return2", "type mismatch")
  checkErrors("examples/return3", "type mismatch")
  checkErrors("examples/return4", "type mismatch")
  checkErrors("examples/return5", "type mismatch")
  checkErrors("examples/return6", "expected integer")
  checkErrors("examples/return7", "type mismatch")
  checkErrors("examples/return8", "missing return in method main.a")
  checkErrors("examples/return9", "main method must be of type void")
  checkOutput("examples/scope", "" -> "ABCDED")
  checkOutput("examples/shortcut1", "" -> "Shortcut!")
  checkOutput("examples/simple")
  checkOutput("examples/truefalse1", "" -> "OK")
  checkErrors("examples/truefalse2", "type mismatch")
  checkErrors("examples/truefalse3", "type mismatch")
  checkErrors("examples/truefalse4", "`BEGIN' expected but TRUE found")
}