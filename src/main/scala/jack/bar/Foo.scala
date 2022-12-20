package jack.bar

import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.types.ScType

object Foo extends ScalaMacroTypeable {
  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {
    println("aas")
    None

  }

  override val boundMacro: Seq[MacroImpl] = Seq.empty
}
