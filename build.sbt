val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalox",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    run / fork := true,
    run / connectInput := true,

    libraryDependencies += "org.scalameta" %% "munit" % "1.2.4" % Test
  )
