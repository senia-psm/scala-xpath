import sbt._
import Keys._

object ScalaXPath extends Build {
  lazy val macros = Project(id = "xpath-macros", base = file("macros")) aggregate(model, parser) dependsOn(model, parser)
  lazy val parser = Project(id = "xpath-parser", base = file("parser")) aggregate(model) dependsOn(model)
  lazy val model = Project(id = "xpath-model", base = file("model"))
}
