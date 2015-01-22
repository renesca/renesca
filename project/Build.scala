import sbt.Keys._
import sbt._

object ApplicationBuild extends Build {

  lazy val RenescaIntegrationTest = config("test-integration") extend (Test)
  lazy val integrationTestSettings : Seq[Setting[_]] = inConfig(RenescaIntegrationTest)(Defaults.testSettings) ++ Seq(
    scalaSource in RenescaIntegrationTest := baseDirectory.value / "src/test-integration/scala",
    parallelExecution in RenescaIntegrationTest := false,
    fork in RenescaIntegrationTest := false
  )
  lazy val project = Project("renesca", file("."))
    .configs(RenescaIntegrationTest)
    .settings(inConfig(RenescaIntegrationTest)(integrationTestSettings): _*)
}
