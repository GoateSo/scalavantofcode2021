val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalacOptions := Seq(
    //   "-Xprint:typer",
      "-color",
      "always"
    ),
  commands += Command.command("cls") { state =>
    print("\u001bc")
    state
  },
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
