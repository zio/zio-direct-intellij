import org.jetbrains.plugins.scala.DependencyManagerBase.RichStr
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.components.libextensions.LibraryExtensionsManager
import org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroTypeable
import org.jetbrains.plugins.scala.lang.typeInference.{TypeInferenceTestBase, WrappingContext}
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

  implicit val wrappingContext = WrappingContext(
    """|import zio._
       |import zio.direct._
       |import java.sql.SQLException
       |
       |case class ConfigA(value: String)
       |case class ConfigB(value: String)
       |case class ConfigC(value: String)
       |case class ConfigD(value: String)
       |case class ConfigE(value: String)
       |
       |class ErrorA extends Exception("A")
       |class ErrorB extends Exception("B")
       |class ErrorC extends Exception("C")
       |class ErrorD extends Exception("D")
       |class ErrorE extends Exception("E")
       |
       |""".stripMargin
  )

  def testServiceWithAttempt(): Unit = {
    doTest(
      s"""defer {
         |  val a = ZIO.service[ConfigA].run.value
         |  val b = ZIO.attempt("foo").run
         |  a
         |}
         |""".stripMargin,
      "ZIO[ConfigA,Throwable,String]"
    )
  }

  def test_defer_info(): Unit = {
    doTest("""defer.info { val a = ZIO.service[ConfigA].run.value; a }""", "ZIO[ConfigA, Nothing, String")
  }
  def test_defer_tpe(): Unit = {
    doTest("""defer.tpe { val a = ZIO.service[ConfigA].run.value; a }""", "ZIO[ConfigA, Nothing, String")
  }
  def test_defer_verbose(): Unit = {
    doTest("""defer.verbose { val a = ZIO.service[ConfigA].run.value; a }""", "ZIO[ConfigA, Nothing, String")
  }
  def test_defer_verboseTree(): Unit = {
    doTest("""defer.verboseTree { val a = ZIO.service[ConfigA].run.value; a }""", "ZIO[ConfigA, Nothing, String")
  }

  def testConfigAndErrorComposition(): Unit = {
    doTest(
      s"""defer {
         |  val a = ZIO.service[ConfigA].run.value
         |  if (a != "foo") ZIO.fail(new SQLException("foo")).run
         |  val b = ZIO.service[ConfigB].run.value
         |  if (b != "bar") ZIO.fail(new IllegalArgumentException("bar")).run
         |  val c = {
         |    val z = defer(123)
         |    z
         |  }
         |  a + "bar"
         |}
         |""".stripMargin,
      "ZIO[ConfigA with ConfigB,Exception,String]"
    )
  }
}