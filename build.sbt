import org.jetbrains.sbtidea.{AutoJbr, JbrPlatform}

lazy val scala213           = "2.13.10"

ThisBuild / intellijPluginName := "intellij-macro-stuff"
ThisBuild / intellijBuild := "223"
ThisBuild / jbrInfo := AutoJbr(explicitPlatform = Some(JbrPlatform.osx_aarch64))

lazy val root =
  newProject("intellij-macro-stuff", file("."))
    .enablePlugins(SbtIdeaPlugin)

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
