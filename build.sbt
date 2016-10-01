//TODO: https://github.com/lihaoyi/acyclic

name := "renesca"

// also change the version in README.md
version in ThisBuild := "1.0.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

crossScalaVersions in ThisBuild := Seq("2.11.8")

lazy val root = project.in(file(".")).
  aggregate(renescaJS, renescaJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

val akkaVersion = "2.4.10"

lazy val renesca = crossProject.in(file("."))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings: _*)
  .settings(sonatypeSettings: _*)
  .jvmSettings(
    libraryDependencies ++= (
      //"io.spray" %% "spray-client" % "1.3.3" ::
      //"io.spray" %% "spray-json" % "1.3.2" ::
      //"com.typesafe.akka" %% "akka-actor" % "2.3.15" ::
      "com.typesafe.akka" %% "akka-actor" % akkaVersion ::
      "com.typesafe.akka" %% "akka-http-core" % akkaVersion ::
      "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion ::
      "com.typesafe.akka" % "akka-http-spray-json-experimental_2.11" % akkaVersion ::
      "org.specs2" %% "specs2-core" % "3.8.4" % "it,test" ::
      ("com.github.httpmock" % "mock-http-server-webapp" % "1.1.9" artifacts (Artifact("mock-http-server-webapp", "jar", "jar")) classifier "") ::
      "com.github.httpmock" %% "httpmock-specs" % "0.6.1" % "it,test" ::
      Nil
    )
  )
  .settings(
    name := "renesca",
    // Scoverage
    scalacOptions in Test ++= Seq("-Yrangepos"),

    // scalaxy (faster collection operations)
    scalacOptions += "-Xplugin-require:scalaxy-streams",
    scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams")),
    scalacOptions in Test += "-Xplugin-disable:scalaxy-streams",
    autoCompilerPlugins := true,
    addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4"),

    // publishing
    pgpSecretRing := file("local.secring.gpg"),
    pgpPublicRing := file("local.pubring.gpg"),
    organization := "com.github.renesca",

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
    },

    initialCommands in console := """
      import renesca.graph._
      import renesca.parameter._
      import renesca.parameter.implicits._
      import renesca._
      import spray.http.BasicHttpCredentials

      val credentials = BasicHttpCredentials("neo4j", "neo4j")
      val restService = new RestService("http://localhost:7474", Some(credentials))

      val db = new DbService
      db.restService = restService
    """,
    resolvers ++= Seq(
      "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases", // to fix specs2 dependency
      "Sonatype" at "https://oss.sonatype.org/content/repositories/releases"
    ),
    scalacOptions ++= (
      "-encoding" :: "UTF-8" ::
      "-unchecked" ::
      "-deprecation" ::
      "-feature" ::
      "-Yinline" :: "-Yinline-warnings" ::
      "-language:_" ::
      //,"-Xdisable-assertions", "-optimize"
      Nil
    )
  )

lazy val renescaJVM = renesca.jvm
lazy val renescaJS = renesca.js
