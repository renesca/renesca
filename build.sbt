name := "neo4j-rest-scala"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.4"

scalacOptions ++= Seq(
"-encoding", "UTF-8",
"-unchecked",
"-deprecation",
"-feature",
"-Yinline", "-Yinline-warnings",
"-language:_"
  //,"-Xdisable-assertions", "-optimize"
)
