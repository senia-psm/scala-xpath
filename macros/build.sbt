name := "scala-xpath-macros"

version := "0.0.1"

scalaVersion := "2.10.1"

sbtVersion := "0.13-SNAPSHOT"

scalacOptions ++= Seq("-deprecation", "-Xlog-free-terms")

//libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies <+= scalaVersion{"org.scala-lang" % "scala-compiler" % _}

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"
