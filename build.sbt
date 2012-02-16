name := "oopsc"

version := "1.0"

organization := "de.martinring"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

mainClass in (Compile, packageBin) := Some("de.martinring.oopsc.App")

mainClass in (Compile, run) := Some("de.martinring.oopsc.App")

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3"

seq(netbeans.NetbeansTasks.netbeansSettings:_*)
