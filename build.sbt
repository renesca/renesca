import SonatypeKeys._


name := "renesca"

version := "0.1.3"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.10.5", "2.11.6")

resolvers ++= Seq(
  "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases", // to fix specs2 dependency
  "Sonatype" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "io.spray" %% "spray-client" % "1.3.2",
  "io.spray" %% "spray-json" % "1.3.1",
  "com.typesafe.akka" %% "akka-actor" % "2.3.8",
  "org.specs2" %% "specs2" % "2.4.17" % "test,test-integration",
  "com.github.httpmock" % "mock-http-server-webapp" % "1.1.9" artifacts (Artifact("mock-http-server-webapp", "jar", "jar")) classifier "",
  "com.github.httpmock" %% "httpmock-specs" % "0.6.1" % "test,test-integration")


// Scoverage
scalacOptions in Test ++= Seq("-Yrangepos")

// publishing
pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

sonatypeSettings

organization := "com.github.renesca"

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
    <developer>
      <id>snordquist</id>
      <name>Sascha Nordquist</name>
      <url>https://github.com/snordquist</url>
    </developer>
    <developer>
      <id>jkaroff</id>
      <name>Johannes Karoff</name>
      <url>https://github.com/cornerman</url>
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
unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaBinaryVersion) {
  (s, v) => s / ("scala_" + v)
}

