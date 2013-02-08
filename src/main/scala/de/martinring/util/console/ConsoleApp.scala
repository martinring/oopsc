package de.martinring.util.console

import de.martinring.util._

class ConsoleApp(val jarName: String, val name: String) extends App { 
  trait Arguments {
    val unrecognized = scala.collection.mutable.Buffer(args :_*)
    var usageTitle = "usage: java -jar " + jarName
    val usageLines = scala.collection.mutable.Buffer[String]()        
    
    def flag(name: String, description: String): Boolean = {
      usageTitle += " [-" + name + "]"
      usageLines += "  -" + name + "       " + description
      val i = unrecognized.indexOf("-" + name) 
      if (i >= 0) unrecognized.remove(i)
      i >= 0
    }
    
    def argument[T](name: String, description: String, optional: Boolean = true)
                   (implicit convert: String => T): Failable[T,String] = {
      usageTitle += (if (optional) " [<" + name + ">]" else " <" + name + ">")
      usageLines += "  <" + name + "> " + description + (if (optional) " (optional)" else "")
      if (!unrecognized.isEmpty) {        
        val res = unrecognized.remove(0)
        Success(convert(res))
      } else Failure(Nil)
    }
    
    def namedArgument[T](name: String, description: String)
                        (implicit convert: String => T): Failable[T,String] = {
      usageTitle += " [-" + name + " i]"
      usageLines += "  -" + name + " i    " + description.format("i")
      val i = unrecognized.indexOf("-" + name)
      if (i >= 0 && unrecognized.length > i + 1) {
        unrecognized.remove(i)
        val res = unrecognized.remove(i)
        Success(convert(res))
      } else Failure(Nil)
    }  
    
    def printUsage() {
      println(usageTitle)
      usageLines.foreach(println)
    }
  }

  def section(name: String) {
    println(List.fill(79)('-').mkString)
    println(" " + name)
    println(List.fill(79)('-').mkString)
    println()
  }
}