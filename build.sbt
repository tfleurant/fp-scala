ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"

lazy val root = (project in file(".")).settings(name := "fp-scala")
libraryDependencies ++= Seq("org.scalameta" %% "munit" % "1.0.0" % Test)
