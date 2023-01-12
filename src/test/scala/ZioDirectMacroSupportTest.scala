import org.jetbrains.plugins.scala.DependencyManagerBase.RichStr
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.components.libextensions.LibraryExtensionsManager
import org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroTypeable
import org.jetbrains.plugins.scala.lang.typeInference.TypeInferenceTestBase
import org.junit.Assert.assertTrue

import java.io.File

class ZioDirectMacroSupportTest extends TypeInferenceTestBase {

  override protected def supportedIn(version: ScalaVersion) =
    version == ScalaVersion.Latest.Scala_2_13

  override protected def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader(("dev.zio" %% "zio-direct" % "1.0.0-RC3").transitive())

  override protected def setUp(): Unit = {
    super.setUp()

    registerScalaPluginExtensions()
  }

  /**
   * This is only needed for tests<br>
   * In production extensions should be registered semi-automatically by reading `META-INF/intellij-compat.json` file in `zio-direct` library
   */
  private def registerScalaPluginExtensions(): Unit = {
    val jarFileWithExtension = new File(getTestDataPath, "../../target/plugin/zio-direct-intellij/lib/zio-direct-intellij.jar")
    assertTrue(s"Jar file doesn't exist: $jarFileWithExtension", jarFileWithExtension.exists())

    val manager = LibraryExtensionsManager.getInstance(getProject)
    //it's registered in the first test run
    val alreadyRegistered = manager.getExtensions[ScalaMacroTypeable].exists(_.isInstanceOf[ScalaMacroTypeable])
    if (!alreadyRegistered) {
      //TODO: make `LibraryExtensionsManager.processResolvedExtension` public in Scala Plugin
      val processResolvedExtension_Method = manager.getClass.getDeclaredMethod("processResolvedExtension", classOf[java.io.File])
      processResolvedExtension_Method.setAccessible(true)
      processResolvedExtension_Method.invoke(manager, jarFileWithExtension)
    }
  }

  def testDummyExampleSuccess(): Unit = {
    doTest(
      s"""val value = "42" + 23
         |${START}value$END
         |//String
         |""".stripMargin
    )
  }

  def testDummyExampleFailure(): Unit = {
    doTest(
      s"""val value = "42" + 23
         |${START}value$END
         |//Short
         |""".stripMargin
    )
  }

  def testMacroSupport(): Unit = {
    doTest(
      s"""object Main {
         |  import zio._
         |  import zio.direct._
         |
         |  val value = defer(123)
         |  ${START}value$END
         |}
         |//ZIO[Any, Nothing, 123]
         |""".stripMargin
    )
  }
}