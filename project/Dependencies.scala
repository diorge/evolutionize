import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.4"
  lazy val scalaTestCheck =
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0"
  lazy val cats = "org.typelevel" %% "cats-core" % "2.7.0"
}
