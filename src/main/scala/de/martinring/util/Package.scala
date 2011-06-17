package de.martinring

import java.io.File

/**
 * @author Martin Ring
 */
package object util {
  implicit def pimpString(s: String) = new {
    def repeat(n: Int) = Iterator.fill(n)(s).mkString

    def fill(params: (String, Any)*) =
      params.foldLeft(s) { case (s, (name, value)) => s.replaceAll("{" + name + "}", value.toString) }

    def toIntOption: Option[Int] = {
      try Some(s.toInt)
      catch { case _ => None }
    }

    def open: File = new File(s)
  }
}