package de.martinring.oopsc

import scala.util.parsing.input.Position

/*
 * Compiler Messages
 * @author Martin Ring
 */
abstract class Message(pos: Position, msg: String, prefix: String = "") {
  override def toString = "[%s] %s: %s\n%s".format(prefix, pos, msg, pos.longString)
}

case class Info(pos: Position, msg: String) extends Message(pos, msg, "Info")
case class Warn(pos: Position, msg: String) extends Message(pos, msg, "Warning")
case class Error(pos: Position, msg: String) extends Message(pos, msg, "Error")