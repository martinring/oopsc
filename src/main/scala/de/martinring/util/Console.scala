package de.martinring.util

object Console {
  private val bufferWidth = 80
  private val separator = List.fill(bufferWidth - 1)('-').mkString
  def section(title: Any) = println('\n' + separator + "\n " + title + '\n' + separator + '\n')
}

class Arguments(private val args: Array[String], usage: String*) {
  private val buffer = args.toBuffer
  
  def printUsage() = { println(usage.map(_+'\n').mkString); sys.exit() }

  def flag(s: String) = if (buffer.contains('-'+s)) { buffer -= '-'+s; true } else false
    
  def option(s: String): Option[String] = if (buffer.contains('-'+s)) {
    val i = buffer.indexOf('-'+s)
    val result = buffer.lift(i + 1)
    buffer.remove(i, 2)
    result
  } else None

  def plain: Option[String] = {
    val result = buffer.collectFirst{ case s if !s.startsWith("-") => s }
    if (result.isDefined) buffer -= result.get
    result
  }

  def fail(msg: String) = { println(msg); sys.exit() }

  def finish() = if (!buffer.isEmpty) {
    printf("Failure: Unknown parameter '%s'. -h for usage\n", buffer.head)
    sys.exit()
  }
}