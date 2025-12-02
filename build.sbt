ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.7"
ThisBuild / name := "advent"
ThisBuild / libraryDependencies := Seq(
  "org.scala-lang" %% "toolkit" % "0.7.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

val source = project.settings(idePackagePrefix := Some("dev.rohenkohl.advent"))