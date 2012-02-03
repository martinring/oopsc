package de.martinring.oopsc

import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

/*
 * Base type for Compiler Messages
 * @author Martin Ring
 * @param pos the position in the source that this message referrs to
 * @param msg the message itself
 * @param prefix the prefix that will be displayed in brackets before the message
 *               (i.e. [Info])
 */
abstract class Message(pos: Position, msg: String, prefix: String = "") {
  override def toString = "[%s] %s: %s\n%s".format(prefix, pos, msg, pos.longString)
}

/* Info message */
case class Info(pos: Position, msg: String) extends Message(pos, msg, "info")
/* Warning */
case class Warn(pos: Position, msg: String) extends Message(pos, msg, "warn")
/* Error Message */
case class Error(pos: Position, msg: String) extends Message(pos, msg, "error")