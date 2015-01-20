import sbt.Keys._
import sbt._

object ApplicationBuild extends Build {
  lazy val RenescaIntegrationTest = config("integration-test") extend (Test)
  lazy val project = Project("renesca", file("."))
    // See http://www.scala-sbt.org/release/docs/Detailed-Topics/Testing#additional-test-configurations-with-shared-sources
    .configs(RenescaIntegrationTest)
    .settings(inConfig(RenescaIntegrationTest)(Defaults.testSettings): _*)
}
