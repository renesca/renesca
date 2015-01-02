import SonatypeKeys._


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



// publishing
pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

sonatypeSettings

profileName := "com.github.renesca"

organization := "com.github.renesca.renesca"


pomExtra := {
  <url>https://github.com/renesca/renesca</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/renesca/renesca</url>
    <connection>scm:git:git@github.com:renesca/renesca.git</connection>
  </scm>
  <developers>
    <developer>
      <id>fdietze</id>
      <name>Felix Dietze</name>
      <url>https://github.com/fdietze</url>
    </developer>
  </developers>
}



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

