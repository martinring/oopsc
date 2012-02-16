import AssemblyKeys._ 

name := "oopsc"

version := "1.0"

organization := "de.martinring"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

mainClass in (Compile, packageBin) := Some("de.martinring.oopsc.App")

mainClass in (Compile, run) := Some("de.martinring.oopsc.App")

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
                  "releases"  at "http://scala-tools.org/repo-releases")
                  
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "de.martinring" %% "util" % "1.0",
  "org.specs2" %% "specs2" % "1.7.1" % "test")  

seq(assemblySettings: _*)