import org.jetbrains.sbtidea.{AutoJbr, JbrPlatform}
import java.nio.file.{FileSystems, Files}

lazy val scala213           = "2.13.10"

ThisBuild / intellijPluginName := "zio-direct-intellij"
ThisBuild / intellijBuild := "223"
ThisBuild / jbrInfo := AutoJbr(explicitPlatform = Some(JbrPlatform.osx_aarch64))
ThisBuild / organization := "dev.zio"
ThisBuild / version := {
  `.zdi.version.override` match {
      case Some(v) => v
      case None => (ThisBuild / version).value
  }
}


def versionFmt(out: sbtdynver.GitDescribeOutput): String = {
//  val dirtySuffix = out.dirtySuffix.dropPlus.mkString("-", "")
//  println(s"--- Dirty Suffix: ${dirtySuffix}")
//  println(s"--- OutputRef: ${out.ref} - OutputRef No Prefix: ${out.ref.dropPrefix}")
  if (out.isCleanAfterTag) {
    println("---- Clean After Tag! ----")
    val v = out.ref.dropPrefix
    println(s"---- DynaVersion: ${v} ----")
    v
  }
  else {
    println("---- NOT Clean After Tag! ----")
    val noPrefixVersion = out.ref.dropPrefix
    val v =
      if (noPrefixVersion.endsWith("-SNAPSHOT"))
        noPrefixVersion
      else
        s"${noPrefixVersion}-SNAPSHOT"
    v
  }
}

def fallbackVersion(d: java.util.Date): String = s"HEAD-${sbtdynver.DynVer timestamp d}"

// foo
inThisBuild(List(
  version := {
    val v = dynverGitDescribeOutput.value.mkVersion(versionFmt, fallbackVersion(dynverCurrentDate.value))
    println(s"---- project version: ${v} ----")
    v
  },
  dynver := {
    val d = new java.util.Date
    val v = sbtdynver.DynVer.getGitDescribeOutput(d).mkVersion(versionFmt, fallbackVersion(d))
    println(s"---- project dynaver: ${v} ----")
    v
  }
))

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


val `.zdi.version.override` = {
  readVersionFromSysProps().orElse(readVersionFromFile()) match {
    case s @ Some(_) => s
    case None =>
      println(s"=== [VERSION] No version from sys-props or file, defaulting to project-based ===")
      None
  }
}

def readVersionFromSysProps() = {
  val version = sys.props.get("zdi.version.override")
  if (version.isDefined) {
    println(s"=== [Project Version] Reading Version from Sys Props: ${version} ===")
  }
  version
}

def readVersionFromFile() = {
  val zdPath = FileSystems.getDefault().getPath(".zdi.version.override")
  if (Files.exists(zdPath)) {
    val strOpt = Files.readAllLines(zdPath).toArray().headOption.map(_.toString)
    strOpt match {
      case Some(v) =>
        println(s"=== [Project Version] Reading Version from .zdi.version.override file: ${v} ===")
        Some(v)
      case None =>
        println("=== [VERSION] Found a .zdi.version.override file but was empty. Skipping ===")
        None
    }
  } else {
    println("=== [VERSION] Could not find a .zdi.version.override file but was empty. Skipping ===")
    None
  }
}