package de.martinring.oopsc

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/*
 * Base type for Compiler Messages
 * @author Martin Ring
 * @param pos the position in the source that this message referrs to
 * @param msg the message itself
 * @param prefix the prefix that will be displayed before the message
 *               (i.e. "Info")
 */
abstract class Message(pos: Position, msg: String, prefix: String = "") {
  override def toString = (pos.line,pos.column) match {
    case (0,0) => "%s: %s".format(prefix,msg)
    case (l,c) => "[%d.%d] %s: %s\n%s".format(l, c, prefix, msg, pos.longString)
  }
}

/* Info message */
case class Info(pos: Position, msg: String) extends Message(pos, msg, "info")
/* Warning */
case class Warn(pos: Position, msg: String) extends Message(pos, msg, "warn")
/* Error Message */
case class Error(pos: Position, msg: String) extends Message(pos, msg, "error")