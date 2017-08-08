// import UnidocKeys._
import ReleaseTransformations._

lazy val buildSettings = Seq(
  organization := "org.tpolecat",
  licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  scalaVersion := "2.12.3",
  crossScalaVersions := Seq("2.11.11", scalaVersion.value),
  scalacOptions in (Compile, doc) += "-no-java-comments"
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
    "-Ywarn-value-discard"
  ),
  scalacOptions in (Compile, doc) ++= Seq(
    "-groups",
    "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
    "-doc-source-url", "https://github.com/tpolecat/tuco/tree/v" + version.value + "€{FILE_PATH}.scala",
    "-skip-packages", "scalaz"
  ),
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)
)

lazy val publishSettings =  Seq(
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  homepage := Some(url("https://github.com/tpolecat/tuco")),
  pomIncludeRepository := Function.const(false),
  pomExtra := (
    <scm>
      <url>git@github.com:tpolecat/tuco.git</url>
      <connection>scm:git:git@github.com:tpolecat/tuco.git</connection>
    </scm>
    <developers>
      <developer>
        <id>tpolecat</id>
        <name>Rob Norris</name>
        <url>http://tpolecat.org</url>
      </developer>
    </developers>
  ),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    ReleaseStep(action = Command.process("package", _)),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _)),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges)
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val tucoSettings = buildSettings ++ commonSettings

lazy val tuco = project
  .in(file("."))
  .settings(tucoSettings)
  .settings(noPublishSettings)
  .dependsOn(wimpi, core, shell, example, docs)
  .aggregate(wimpi, core, shell, example, docs)

lazy val core = project
  .in(file("modules/core"))
  .dependsOn(wimpi)
  .settings(name := "tuco-core")
  .settings(tucoSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-core"   % "1.0.0-MF",
      "org.typelevel"  %% "cats-free"   % "1.0.0-MF",
      "org.typelevel"  %% "cats-effect" % "0.4"
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
    libraryDependencies += "com.monovore" %% "decline" % "0.2.2"
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
    )
  )
