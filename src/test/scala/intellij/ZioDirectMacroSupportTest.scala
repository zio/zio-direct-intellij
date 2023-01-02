package intellij

import org.jetbrains.plugins.scala._
import org.jetbrains.plugins.scala.annotator._
import org.jetbrains.plugins.scala.annotator.template._
import com.intellij.openapi.extensions.{ExtensionPoint, Extensions}
import org.jetbrains.plugins.scala.annotator.element.ScTemplateDefinitionAnnotator
import org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroTypeable
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
//import org.jetbrains.plugins.scala.lang.typeInference.testInjectors.{SCL9446Injector, SCL9446InjectorNoOverride}

import scala.annotation.nowarn


class ZioDirectMacroSupportTest extends AnnotatorTestBase[ScTemplateDefinition] {

//  def testSCL2981(): Unit = {
//    assertMatches(messages("trait A { type T; def t(p: T)}; class B extends A { type T = Int; override def t(p: T) = ()}")) {
//      case Nil =>
//    }
//  }
//
//  def testSCL3515(): Unit = {
//    assertMatches(messages("trait A { type T}; class B extends A")) {
//      case Nil =>
//    }
//  }


  def testSCL3514(): Unit = {
    val code =
"""
import zio._
import zio.direct._
val v = defer(123)
"""
//    assertMatches(messages(code)) {
//      case list => println(s"----------- List: ${list}")
//    }

    doInjectorTest(new ZioDirectMacroSupport) {
      val msg = messages(code)
      println(msg)
    }
  }

//  def testSCL4258(): Unit = {
//    val code =
//      """
//        |abstract class Parent {
//        |  def m(p: T forSome {type T})
//        |}
//        |class Child extends Parent {
//        |  def m(p: T forSome {type T}) { }
//        |}
//      """.stripMargin
//    assertMatches(messages(code)) {
//      case Nil =>
//    }
//  }
//
//  def testSCL9446(): Unit = {
//    doInjectorTest(new SCL9446Injector) {
//      val code =
//        """
//          |object ppp {
//          |  trait A {
//          |    def foo(): Int
//          |  }
//          |
//          |  class B extends A {
//          |  }
//          |}
//        """.stripMargin
//      assertMatches(messages(code)) {
//        case Nil =>
//      }
//    }
//  }
//
//  def testSCL9446NoOverride(): Unit = {
//    doInjectorTest(new SCL9446InjectorNoOverride) {
//      val code =
//        """
//          |object ppp {
//          |  trait A {
//          |    def foo(): Int
//          |  }
//          |
//          |  class B extends A {
//          |  }
//          |}
//        """.stripMargin
//      assertMatches(messages(code)) {
//        case Nil =>
//      }
//    }
//  }

  override protected def annotate(element: ScTemplateDefinition)
    (implicit holder: ScalaAnnotationHolder): Unit =
    ScTemplateDefinitionAnnotator.annotateNeedsToBeAbstract(element)

  private def doInjectorTest(injector: ScalaMacroTypeable)(body: => Unit): Unit = {
    Extensions.getRootArea.registerExtensionPoint(
      "macroTypeable",
      "org.jetbrains.plugins.scala.lang.macros.evaluator.ScalaMacroTypeable",
      ExtensionPoint.Kind.INTERFACE
    )
    val extensionPoint = Extensions.getRootArea.getExtensionPoint[ScalaMacroTypeable]("macroTypeable")
    extensionPoint.registerExtension(injector)
    try {
      body
    }
    finally {
      extensionPoint.unregisterExtension(injector)
    }
  }
}