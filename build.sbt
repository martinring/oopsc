name := "oopsc"

version := "1.0"

organization := "de.martinring"

scalaVersion := "2.10.0-M2"

scalacOptions += "-deprecation"

mainClass in (Compile, packageBin) := Some("de.martinring.oopsc.App")

mainClass in (Compile, run) := Some("de.martinring.oopsc.App")

libraryDependencies += "de.martinring" % "util_2.10.0-M2" % "1.0"