name := "scala-xpath-parser"

version := "0.0.1"

scalaVersion := "2.10.1"

sbtVersion := "0.13-SNAPSHOT"

parallelExecution in Test := false

scalacOptions += "-deprecation"

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"
