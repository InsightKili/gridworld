ThisBuild / organization := "com.InsightsKili"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version      := "0.1.0-SNAPSHOT"

bloopExportJarClassifiers in Global := Some(Set("sources"))

lazy val gridworld = (project in file("main"))
  .settings(
    name := "gridworld"
  )
