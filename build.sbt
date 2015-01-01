name := "renesca"

version := "0.1"

scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.10.4", "2.11.4")

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.3.1",
  "org.specs2" %% "specs2" % "2.4.2" % "test"
)

// Scoverage
scalacOptions in Test ++= Seq("-Yrangepos")


scalacOptions ++= Seq(
"-encoding", "UTF-8",
"-unchecked",
"-deprecation",
"-feature",
"-Yinline", "-Yinline-warnings",
"-language:_"
  //,"-Xdisable-assertions", "-optimize"
)

// support source folders for version specific code
unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaBinaryVersion){
    (s, v) => s / ("scala_"+v)
}

