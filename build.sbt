//TODO: scalaxy-streams
//TODO: https://github.com/lihaoyi/acyclic

name := "renesca"

// also change the version in README.md
version in ThisBuild := "0.3.2-9"

scalaVersion in ThisBuild := "2.11.8"

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8")

lazy val root = project.in(file(".")).
  aggregate(renescaJS, renescaJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val IntegrationTest = config("test-integration") extend (Test)
lazy val renesca = crossProject.in(file("."))
  .configs(IntegrationTest)
  .settings(
    scalaSource in IntegrationTest := baseDirectory.value / "src/test-integration/scala",
    parallelExecution in IntegrationTest := false,
    fork in IntegrationTest := false
  )
  .settings(sonatypeSettings: _*)
  .settings(
    name := "renesca",
    libraryDependencies ++= (
      "io.spray" %% "spray-client" % "1.3.3" ::
      "io.spray" %% "spray-json" % "1.3.2" ::
      "com.typesafe.akka" %% "akka-actor" % "2.3.15" ::
      "org.specs2" %% "specs2-core" % "3.8.4" % "test,test-integration" ::
      ("com.github.httpmock" % "mock-http-server-webapp" % "1.1.9" artifacts (Artifact("mock-http-server-webapp", "jar", "jar")) classifier "") ::
      "com.github.httpmock" %% "httpmock-specs" % "0.6.1" % "test,test-integration" ::
      Nil
    ),
    // Scoverage
    scalacOptions in Test ++= Seq("-Yrangepos"),

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
  .jvmSettings()
  .jsSettings()

lazy val renescaJVM = renesca.jvm
lazy val renescaJS = renesca.js
