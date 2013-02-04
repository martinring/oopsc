package de.martinring.util.console

import de.martinring.util._

class ConsoleApp(val jarName: String, val name: String) extends App { 
  trait Arguments {
    val unrecognized = scala.collection.mutable.Buffer(args :_*)
    var usageTitle = "usage: java -jar " + jarName
    var usageLines = List[String]()
    
    def flag(name: String, description: String): Boolean = {
      val i = unrecognized.indexOf("-" + name) 
      if (i >= 0) unrecognized.remove(i)
      i >= 0
    }
    
    def argument[T](name: String, description: String, optional: Boolean = true)
                   (implicit convert: String => T): Failable[T,String] = {      
      if (!unrecognized.isEmpty) {        
        val res = unrecognized.remove(0)
        Success(convert(res))
      } else Failure(Nil)
    }
    
    def namedArgument[T](name: String, description: String)
                        (implicit convert: String => T): Failable[T,String] = {
      val i = unrecognized.indexOf("-" + name)
      if (i >= 0 && unrecognized.length > i + 1) {
        unrecognized.remove(i)
        val res = unrecognized.remove(i)
        Success(convert(res))
      } else Failure(Nil)
    }  
  }

  def section(name: String) {
    println(List.fill(80)('-').mkString)
    println(name)
    println(List.fill(80)('-').mkString)
  }
}