import org.jetbrains.sbtidea.{AutoJbr, JbrPlatform}
import java.nio.file.{FileSystems, Files}

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.dev/zio-direct-intellij")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "deusaquilus",
        "Alexander Ioffe",
        "deusaquilus@gmail.com",
        url("https://github.com/deusaquilus")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),

    intellijPluginName := "zio-direct-intellij",
    intellijBuild := "223",
    jbrInfo := AutoJbr(explicitPlatform = Some(JbrPlatform.osx_aarch64)),
    organization := "dev.zio"
  ) ++ {
    `.zdi.version.override` match {
        case Some(v) =>
          List(version := v)
        case None =>
          List()
    }
  }
)

lazy val scala213           = "2.13.10"



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

// Test Change
