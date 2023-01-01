package intellij

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.{Qualified, invocation}
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.{Any, Nothing, ParameterizedType}
import Extractors._

class ZioDirectMacroSupport extends ScalaMacroTypeable {

  sealed trait UnionType
  object UnionType {
    case object LUB extends UnionType
    case object Or extends UnionType
  }

  sealed trait TypeComputation
  object TypeComputation{
    case class Success(zioMod: ZioMod) extends TypeComputation
    case object Failure extends TypeComputation
  }

  case class ZioDirectContext(unionType: UnionType, context: MacroContext)

  class ZioMod private (val r: ScType, val e: ScType) {
    import ZioMod._
    def compseWith(other: ZioMod)(implicit zdc: ZioDirectContext) = {
      for {
        andType <- and(this.r, other.r)
        orType <- or(this.e, other.e)
      } yield new ZioMod(andType, orType)
    }

    def toZio(valueType: ScType)(implicit zdc: ZioDirectContext): Option[ScType] = {
      val typeText = s"_root_.zio.ZIO[${r.canonicalText}, ${e.canonicalText}, ${valueType.canonicalText}]"
      ScalaPsiElementFactory.createTypeFromText(typeText, zdc.context.place, null)
    }

    override def toString: String = s"ZioMod($r, $e)"
  }

  object ZioMod {
    def default(implicit zdc: ZioDirectContext) = {
      implicit val place = zdc.context.place.getProject
      new ZioMod(Any, Nothing)
    }

    def unapply(scType: ScType) = {
      // TODO do we need to do a .withImplicitConversions here?
      val dealiased = if (scType.isAliasType)  scType.removeAliasDefinitions() else scType
      if (dealiased.canonicalText.startsWith("_root_.zio.ZIO["))
        dealiased match {
          case par: ParameterizedType =>
            val args = par.typeArguments
            Some(new ZioMod(args(0), args(1)))
          case _ => None
        }
      else
        None
    }

    private def and(a: ScType, b: ScType)(implicit zdc: ZioDirectContext) = {
      implicit val place = zdc.context.place.getProject
      if (a.isAny && b.isAny) Some(Any)
      else if (a.isAny) Some(b)
      else if (b.isAny) Some(a)
      else
        ScalaPsiElementFactory
          .createTypeElementFromText(s"${a.canonicalText} with ${b.canonicalText}", zdc.context.place, null)
          .`type`().toOption
    }

    private def or(a: ScType, b: ScType)(implicit zdc: ZioDirectContext) =
      zdc.unionType match {
        case UnionType.LUB =>
          Some(a.lub(b))
        case UnionType.Or =>
          ScalaPsiElementFactory
            .createTypeElementFromText(s"${a.canonicalText}|${b.canonicalText}", zdc.context.place, null)
            .`type`().toOption

      }

  }

  object StructureSupport {
    // TODO For defer in defer, assume that the outer one has run first and it's type is correct so just write
    //      the output type of inner defers into the outer context
    private def findFirstDeferCallBody(element: PsiElement): Option[ScExpression] =
      element match {
        // note, element should also be an instance of ScMethodCall but don't need that information here because
        // MethodRepr in StaticMemberReferenceExtractor takes care of it
        case `defer._`(body) =>
          Some(body)
        case _ =>
          element.getChildren.view.map(findFirstDeferCallBody(_)).find(_.isDefined).flatten
    }

    def firstDeferCallDetails(context: MacroContext) =
      for {
        deferBody <-
          StructureSupport.findFirstDeferCallBody(context.place)
        // TODO need to check invocatios of defer.info etc... and also when a withAbstractError is used to get the least upper bound for errors
        //      if this override is specified for Scala 3
        deferBodyType <-
          deferBody.`type`().toOption
      } yield (deferBody, deferBodyType)
  }

  // TODO do we need to skip runs in nested defers? check that we don't go into them???
  //      need to see an example of that
  def findRunTypes(element: PsiElement): List[Option[ScType]] =
    element match {
      case `.run from ZioRunOps`(arg) =>
        List(arg.`type`().toOption)
      case _ =>
        element.getChildren.flatMap(findRunTypes(_)).toList
    }

  object CommonExt {
    implicit class ListOptionExt[T](t: List[Option[T]]) {
      def allOrNothing: Option[List[T]] = {
        if (t.isEmpty) None
        else if (t.forall(_.isDefined)) Some(t.map(_.get))
        else None
      }
    }
  }
  import CommonExt._

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {


    implicit val ctx =
      ZioDirectContext(
        if (macros.isInScala3Module) UnionType.Or else UnionType.LUB,
        context
      )

    def findAndComposeRunTypes(bodyType: ScType, body: ScExpression): Option[ScType] = {
      for {
        runScTypes <- {
          // NOTE all of this is assuming that the environment and error parameters are associative
          // up to down (in multiple lines of code) and left to right (in the same line of code)
          // based on the flatMap-chain structure produced by zio-direct I believe this is the lawful behavior
          findRunTypes(body).allOrNothing
        }
        totalModType <- {
          val runTypesOpt = runScTypes.map(ZioMod.unapply(_))
          runTypesOpt.reduce((a, b) => {
            // if both are defined, compose them, the composition can also be none
            for {
              aVal <- a
              bVal <- b
              result <- aVal.compseWith(bVal)
            } yield result
          })
        }
        totalZioType <- totalModType.toZio(bodyType.widen)
      } yield totalZioType
    }

    val totalType =
      for {
        (body, bodyType) <-
          StructureSupport.firstDeferCallDetails(context)
        totalZioType <-
          findAndComposeRunTypes(bodyType, body)
            .orElse(ZioMod.default.toZio(bodyType))
      } yield totalZioType

    println(s"========== TotalType: ${totalType}")
    totalType
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
