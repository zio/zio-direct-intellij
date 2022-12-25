package intellij

import org.jetbrains.plugins.scala.codeInspection.collections.{Qualified, invocation}
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaRecursiveElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.types.ScType

class ZioDirectMacroSupport extends ScalaMacroTypeable {

  val `.run`: Qualified = invocation("run").from(Seq("zio.direct.ZioRunOps"))

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {
    val visitor = new ScalaRecursiveElementVisitor {
      override def visitReferenceExpression(ref: ScReferenceExpression): Unit = {
        ref match {
          case x @ `.run` => println(x)
          case _ =>
        }
      }
    }
    context.place.accept(visitor)
    None
  }

  override val boundMacro: Seq[MacroImpl] =
    MacroImpl("apply", "zio.direct.defer") ::
      Nil

  /*
  MacroImpl("product", "shapeless.Generic") ::
    MacroImpl("apply", "shapeless.LowPriorityGeneric") ::
    Nil
   */
}
