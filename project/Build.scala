import sbt._
import Keys._

object Settings {

  val paradiseVersion   = "2.1.0-M5"

  val scalaBuildVersion = "2.11.6"

  val common = Defaults.defaultSettings ++ Seq(
    organization := "de.unituebingen",
    scalaVersion := "2.11.6",
    version := "0.1-SNAPSHOT",

    scalacOptions := Seq(
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked"),

    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
    initialCommands in console := """import objectalgebras._"""
  )
}

object ObjectAlgebrasBuild extends Build {

  lazy val objectalgebras = (project in file(".")
    aggregate (core, examples)
    dependsOn (core, examples)
    settings (Settings.common: _*)
    settings (
      moduleName := "objectalgebras-root",

      publish := (),
      publishLocal := ()
    )
  )

  lazy val core = (project
    settings (Settings.common: _*)
    settings (
      moduleName := "objectalgebras",

      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
      ),

      libraryDependencies ++= (if (scalaVersion.value.startsWith("2.10"))
          List("org.scalamacros" %% "quasiquotes" % Settings.paradiseVersion)
        else
          Nil)

    )
  )

  lazy val examples = (project
    dependsOn core
    settings (Settings.common: _*)
    settings (
      moduleName := "objectalgebras-examples",
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      publish := (),
      publishLocal := ()
    )
  )
}
