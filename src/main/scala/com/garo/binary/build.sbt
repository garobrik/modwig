val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "binary",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scodec" %% "scodec-core" % "2.2.2",
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0",
  )
