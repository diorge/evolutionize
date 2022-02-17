import Dependencies._

ThisBuild / scalaVersion := "3.1.1"
ThisBuild / version := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "evolutionize",
    libraryDependencies ++= Seq(
      cats,
      scalaTest % Test,
      scalaCheck % Test,
      scalaTestCheck % Test
    )
  )
