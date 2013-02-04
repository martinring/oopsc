name := "oopsc"

version := "1.0"

organization := "de.martinring"

scalaVersion := "2.10.0"

scalacOptions += "-deprecation"

mainClass in (Compile, packageBin) := Some("de.martinring.oopsc.App")

mainClass in (Compile, run) := Some("de.martinring.oopsc.App")

libraryDependencies += "org.specs2" %% "specs2" % "1.13" % "test"