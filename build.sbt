import org.typelevel.scalacoptions.ScalacOptions

import ReleaseTransformations._

ThisBuild / scalaVersion         := "3.3.4"
ThisBuild / organization         := "com.dedipresta"
ThisBuild / organizationName     := "Dedipresta"
ThisBuild / startYear            := Some(2025)
ThisBuild / organizationHomepage := Some(url("https://www.dedipresta.com"))
ThisBuild / scalafmtOnCompile    := true
ThisBuild / crossScalaVersions   := List("3.3.4")

ThisBuild / developers := List(
  Developer(
    "mprevel",
    "Mathieu Prevel",
    "contact@dedipresta.com",
    url("https://www.dedipresta.com"),
  ),
)

ThisBuild / homepage          := Some(url("https://github.com/dedipresta/srules"))
ThisBuild / scmInfo           := Some(ScmInfo(url("https://github.com/dedipresta/srules"), "git@github.com:dedipresta/srules.git"))
ThisBuild / publishTo         := Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)
ThisBuild / licenses          := List("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / publishMavenStyle := true
ThisBuild / releaseCrossBuild := true
ThisBuild / releaseProcess    := Seq[ReleaseStep](
  checkSnapshotDependencies,                        // check that there is no SNAPSHOT dependencies
  inquireVersions,                                  // ask user to enter the current and next version
  runClean,                                         // clean
  runTest,                                          // run tests
  setReleaseVersion,                                // set release version in version.sbt
  commitReleaseVersion,                             // commit the release version
  tagRelease,                                       // create git tag
  releaseStepCommandAndRemaining("+publishSigned"), // run +publishSigned command to sonatype stage release
  setNextVersion,                                   // set next version in version.sbt
  commitNextVersion,                                // commit next version
  releaseStepCommand("sonatypeRelease"),            // run sonatypeRelease and publish to maven central
  pushChanges,                                      // push changes to git
)

val catsCore  = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "1.1.0")
val circe     = Def.setting("io.circe" %%% "circe-parser" % "0.14.11")
val munit     = Def.setting("org.scalameta" %%% "munit" % "1.1.0")

lazy val commonLibraryDependencies = Def.setting(
  Seq(
    munit.value,
  ),
)

lazy val commonLibrarySettings = Seq(
  coverageFailOnMinimum      := true,
  libraryDependencies ++= commonLibraryDependencies.value,
  Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement,
  coverageMinimumStmtTotal   := 85,
  coverageMinimumBranchTotal := 85,
)

lazy val jsSettings = Seq(
  coverageEnabled := false,
)

lazy val srules = project
  .in(file("."))
  .settings(
    publish / skip             := true,
    coverageMinimumStmtTotal   := 95,
    coverageMinimumBranchTotal := 95,
  )
  .aggregate(
    `srules-core`.jvm,
    `srules-core`.js,
    `srules-eval`.jvm,
    `srules-eval`.js,
    `srules-logic`.jvm,
    `srules-logic`.js,
    `srules-logic-circe`.jvm,
    `srules-logic-circe`.js,
  )

lazy val `srules-core` = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("srules-core"))
  .settings(
    name                       := "srules-core",
    description                := "Rules parsing library",
    commonLibrarySettings,
    libraryDependencies ++= Seq(
      catsParse.value,
    ),
    coverageMinimumBranchTotal := 85, // scala.js toString on floating point numbers
  )
  .jsSettings(jsSettings)

lazy val `srules-eval` = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("srules-eval"))
  .settings(
    name        := "srules-eval",
    description := "Rules evaluation library",
    commonLibrarySettings,
    libraryDependencies ++= Seq(
    ),
  )
  .dependsOn(`srules-core`)
  .jsSettings(jsSettings)

lazy val `srules-logic` = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("srules-logic"))
  .settings(
    name        := "srules-logic",
    description := "Rules logic library",
    commonLibrarySettings,
    libraryDependencies ++= Seq(
    ),
  )
  .dependsOn(`srules-core`, `srules-eval`)
  .jsSettings(jsSettings)

lazy val `srules-logic-circe` = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("srules-logic-circe"))
  .settings(
    name        := "srules-logic-circe",
    description := "Rules logic JSON support with Circe",
    commonLibrarySettings,
    libraryDependencies ++= Seq(circe.value),
  )
  .dependsOn(`srules-core`, `srules-logic`)
  .jsSettings(jsSettings)
