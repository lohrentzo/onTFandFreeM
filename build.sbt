import Dependencies._

ThisBuild / scalaVersion     := "2.11.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)

lazy val root = (project in file("."))
  .settings(
    name := "onTFandFreeM",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.1",
    libraryDependencies += "org.typelevel" %% "cats-free" % "1.6.1"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
