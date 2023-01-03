import org.jetbrains.sbtidea.{AutoJbr, JbrPlatform}
import java.nio.file.{FileSystems, Files}

lazy val scala213           = "2.13.10"

ThisBuild / intellijPluginName := "intellij-macro-stuff"
ThisBuild / intellijBuild := "223"
ThisBuild / jbrInfo := AutoJbr(explicitPlatform = Some(JbrPlatform.osx_aarch64))

ThisBuild / organization := "dev.zio"

addCommandAlias("deploy", "deleteCache; publishLocal")

lazy val root =
  newProject("zio-direct-intellij", file("."))
    .enablePlugins(SbtIdeaPlugin)

lazy val deleteCache = taskKey[Boolean]("Delete SBT Cache, returns false if cache didn't exist")
deleteCache := {
  import scala.sys.process._
  val fs = FileSystems.getDefault
  val homeDir = sys.env.get("HOME").getOrElse { throw new RuntimeException("Could not find home dir") }
  val cacheFile = fs.getPath(homeDir, ".ivy2", "cache", "dev.zio", "zio-direct-intellij_2.13")
  val exists = Files.exists(cacheFile)
  if (exists) {
    val command = s"rm -rf ${cacheFile}"
    println(s"-- Execute: ${command}")
    command !;
    println(s"----- The cache was deleted: ${cacheFile} -----")
  } else
    println(s"----- The cache could not be found: ${cacheFile} -----")
  exists
}

def newProject(projectName: String, base: File): Project =
  Project(projectName, base).settings(
    name := projectName,
    scalaVersion := scala213,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      "org.intellij.scala".toPlugin
    ),
    (Test / scalacOptions) += "-Xmacro-settings:enable-expression-tracers"
  )
