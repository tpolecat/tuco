import UnidocKeys._
import ReleaseTransformations._

lazy val buildSettings = Seq(
  organization := "org.tpolecat",
  licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", scalaVersion.value)
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
    "-Xlint",
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
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
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
  .dependsOn(wimpi, core)
  .aggregate(wimpi, core)

lazy val core = project
  .in(file("modules/core"))
  .dependsOn(wimpi)
  .settings(name := "tuco-core")
  .settings(tucoSettings)
  .settings(publishSettings)
  .settings(
    resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven",
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core"                % "7.2.3",
      "org.scalaz"  %% "scalaz-effect"              % "7.2.3",
      "net.bmjames" %% "scala-optparse-applicative" % "0.4"
    )
  )

lazy val wimpi = project
  .in(file("modules/wimpi"))
  .settings(
    name := "tuco-wimpi",
    compileOrder := CompileOrder.JavaThenScala
  )
  .settings(tucoSettings)
  .settings(publishSettings)

lazy val docs = project
  .in(file("modules/docs"))
  .settings(tucoSettings)
  .settings(noPublishSettings)
  .dependsOn(core)
