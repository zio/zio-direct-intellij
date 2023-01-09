package org.jetbrains.plugins.scala.base.libraryLoaders

import com.intellij.openapi.module.Module
import org.jetbrains.plugins.scala.ScalaVersion

trait LibraryLoader {
  def init(implicit module: Module, version: ScalaVersion): Unit

  def clean(implicit module: Module): Unit = ()
}
