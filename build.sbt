

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
)

lazy val tuco = project
  .in(file("."))
  .settings(scalaVersion := "2.11.8")
  .aggregate(wimpi, core)

lazy val core = project
  .in(file("modules/core"))
  .dependsOn(wimpi)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core"                % "7.2.3",
      "org.scalaz"  %% "scalaz-effect"              % "7.2.3",
      "net.bmjames" %% "scala-optparse-applicative" % "0.4"
    )
  )

lazy val wimpi = project
  .in(file("modules/wimpi"))
  .settings(commonSettings)
