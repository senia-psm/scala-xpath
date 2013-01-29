import sbt._
import Keys._

object ScalaXPath extends Build {
  lazy val parser = Project(id = "xpath-parser", base = file("parser")) aggregate(model) dependsOn(model)
  lazy val model = Project(id = "xpath-model", base = file("model"))
}
