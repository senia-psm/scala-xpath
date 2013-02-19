name := "scala-xpath-parser"

version := "0.0.1"

scalaVersion := "2.10.0"

parallelExecution in Test := false

scalacOptions += "-deprecation"

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalatest" %% "scalatest" % "[1.7.2,)" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"
