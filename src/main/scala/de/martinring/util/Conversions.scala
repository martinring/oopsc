package de.martinring.util

import java.io.File

object Conversions {
  implicit val stringAsInt:  String => Int  = (string => string.toInt)
  implicit val stringAsFile: String => File = (string => new File(string))
}