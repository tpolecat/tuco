import ReleaseTransformations._
import microsites._

// Library versions all in one place, for convenience and sanity.
lazy val catsVersion          = "1.4.0"
lazy val catsEffectVersion    = "1.0.0"
lazy val declineVersion       = "0.5.1"
lazy val kpVersion            = "0.9.8"
lazy val monocleVersion       = "1.5.1-cats"
lazy val paradiseVersion      = "2.1.1"
lazy val scala211Version      = "2.11.11"
lazy val scala212Version      = "2.12.7"

// run dependencyUpdates whenever we [re]load. Spooky eh?
onLoad in Global := { s => "dependencyUpdates" :: s }

lazy val buildSettings = Seq(
  organization := "org.tpolecat",
  licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  scalaVersion := scala212Version,
  crossScalaVersions := Seq(scala211Version, scalaVersion.value),
  resolvers += Resolver.jcenterRepo
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-encoding", "UTF-8", // 2 args
    "-feature",
    "-deprecation",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    // "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard",
    "-Ypartial-unification",
    "-Yrangepos"
  ),
  scalacOptions in (Compile, doc) ++= Seq(
    "-groups",
    "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
    "-doc-source-url", "https://github.com/tpolecat/tuco/tree/v" + version.value + "€{FILE_PATH}.scala",
    "-skip-packages", "scalaz"
  ),
  addCompilerPlugin("org.spire-math"  %  "kind-projector" % kpVersion       cross CrossVersion.binary),
  addCompilerPlugin("org.scalamacros" %% "paradise"       % paradiseVersion cross CrossVersion.patch),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  releaseProcess := Nil
)

lazy val publishSettings =  Seq(
  useGpg := false,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  homepage := Some(url("https://github.com/tpolecat/tuco")),
  pomIncludeRepository := Function.const(false),
  pomExtra := (
    <developers>
      <developer>
        <id>tpolecat</id>
        <name>Rob Norris</name>
        <url>http://tpolecat.org</url>
      </developer>
    </developers>
  ),
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val noPublishSettings = Seq(
  skip in publish := true
)

lazy val tucoSettings = buildSettings ++ commonSettings

lazy val tuco = project
  .in(file("."))
  .settings(tucoSettings)
  .settings(noPublishSettings)
  .dependsOn(wimpi, core, shell, example, docs)
  .aggregate(wimpi, core, shell, example, docs)
  .settings(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      releaseStepCommand("docs/tut"), // annoying that we have to do this twice
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      releaseStepCommand("sonatypeReleaseAll"),
      releaseStepCommand("docs/publishMicrosite"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )

lazy val core = project
  .in(file("modules/core"))
  .dependsOn(wimpi)
  .settings(name := "tuco-core")
  .settings(tucoSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"              %% "cats-core"     % catsVersion,
      "org.typelevel"              %% "cats-free"     % catsVersion,
      "org.typelevel"              %% "cats-effect"   % catsEffectVersion,
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion  % "test"
    )
  )

lazy val shell = project
  .in(file("modules/shell"))
  .dependsOn(core)
  .settings(name := "tuco-shell")
  .settings(tucoSettings)
  .settings(publishSettings)
  .settings(
    resolvers += Resolver.bintrayRepo("bkirwi", "maven"),
    libraryDependencies += "com.monovore" %% "decline" % declineVersion
  )

lazy val wimpi = project
  .in(file("modules/wimpi"))
  .settings(
    name := "tuco-wimpi",
    compileOrder := CompileOrder.JavaThenScala
  )
  .settings(tucoSettings)
  .settings(publishSettings)

lazy val example = project
  .in(file("modules/example"))
  .settings(tucoSettings)
  .settings(noPublishSettings)
  .dependsOn(shell)

lazy val docs = project
  .in(file("modules/docs"))
  .dependsOn(shell)
  .settings(tucoSettings)
  .settings(noPublishSettings)
  .enablePlugins(MicrositesPlugin)
  .settings(
    micrositeName             := "tuco",
    micrositeDescription      := "Tuco is a reasonable telnet server for Scala.",
    micrositeAuthor           := "Rob Norris",
    micrositeGithubOwner      := "tpolecat",
    micrositeGithubRepo       := "tuco",
    micrositeGitterChannel    := false, // no me gusta
    micrositeBaseUrl          := "/tuco",
    micrositeDocumentationUrl := "/tuco/docs/",
    micrositeHighlightTheme   := "color-brewer",
    micrositePalette := Map(
      "brand-primary"     -> "#0B6E0B",
      "brand-secondary"   -> "#084D08",
      "brand-tertiary"    -> "#053605",
      "gray-dark"         -> "#453E46",
      "gray"              -> "#837F84",
      "gray-light"        -> "#E3E2E3",
      "gray-lighter"      -> "#F4F3F4",
      "white-color"       -> "#FFFFFF"
    ),
    micrositeConfigYaml := ConfigYml(
      yamlCustomProperties = Map(
        "tucoVersion"       -> version.value,
        "catsVersion"       -> catsVersion,
        "catsEffectVersion" -> catsEffectVersion,
        "declineVersion"    -> declineVersion,
        "monocleVersion"    -> monocleVersion,
        "scalaVersions"     -> crossScalaVersions.value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", "") // 2.11/12
      )
    )
  )
