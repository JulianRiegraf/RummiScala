import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "de.htwg.se"
ThisBuild / organizationName := "JuPa"

lazy val root = (project in file("."))
  .settings(
    name := "RummiScala",
    libraryDependencies += scalaTest % Test
  )



libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.12" % "2.0.3"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
