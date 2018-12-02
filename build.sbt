name := "adventOfCode2018"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

scalafmtOnCompile := true

scalafmtOnCompile.in(Sbt) := true

scalafmtVersion := "1.4.0"