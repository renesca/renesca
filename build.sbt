name := "renesca"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4.2" % "test"
)

// Scoverage
scalacOptions in Test ++= Seq("-Yrangepos")

coverallsSettings

scalacOptions ++= Seq(
"-encoding", "UTF-8",
"-unchecked",
"-deprecation",
"-feature",
"-Yinline", "-Yinline-warnings",
"-language:_"
  //,"-Xdisable-assertions", "-optimize"
)
