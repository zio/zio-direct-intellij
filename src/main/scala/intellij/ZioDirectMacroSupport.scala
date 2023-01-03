package intellij

import com.intellij.psi.PsiElement
import intellij.Extractors._
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaRecursiveElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.api.{Any, Nothing, ParameterizedType}

import scala.collection.mutable.ArrayBuffer

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

    def unapply(scTypeRaw: ScType) = {
      // Widen any singleton types for ZIOs captured in a property
      // e.g. if there's something like `val v = ZIO.succeed(1); "foo" + v.run`
      // the `v.run` would be typed as v.type instead of ZIO[Any, Nothing, Int]
      // fix this.
      val scType = scTypeRaw.widen

      val dealiased =
        if (scType.isAliasType)  scType.removeAliasDefinitions() else scType

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
          // Unfortunately IntelliJ doesn't actually support union types yet (see https://youtrack.jetbrains.com/issue/SCL-16148/support-for-union-types-syntax)
          // so we are forced to do least-upper-bounds.
          // TODO Probably should have a intellij warning that zio-direct should be configured to use LUB instead of Or-types in Scala3
          Some(a.lub(b))

          // val feat = ScalaFeatures.onlyByVersion(ScalaVersion.fromString("3.2.0").get)
          // val elem = ScalaPsiElementFactory.createTypeElementFromText(s"String|Int", feat)(zdc.context.place.getProject).`type`()
          // if (a.isNothing && b.isNothing) Some(a)
          // else if (a.isNothing) Some(b)
          // else if (b.isNothing) Some(a)
          // else
          //   ScalaPsiElementFactory.createTypeElementFromText(s"${a.canonicalText}|${b.canonicalText}", feat)(zdc.context.place.getProject)
          //     .`type`().toOption
      }

  }

  object StructureSupport {
    // For defer in defer, assume that the outer one has run first and it's type is correct so just write
    // the output type of inner defers into the outer context
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
        deferBodyType <-
          deferBody.`type`().toOption
      } yield (deferBody, deferBodyType)
  }

  // For now don't need to think about `run(run(stuff))` since that is not allowed.
  // For run(defer(run)) cases the inner defers will be resolve before the outer ones so do not need
  // to worry about that case either.
  def findRunTypes(element: PsiElement): List[Option[ScType]] = {
    val buff = new ArrayBuffer[Option[ScType]]()
    // use a visitor instead of manually recursing on .getChildren because the latter can lead to infinite loops
    val visitor = new ScalaRecursiveElementVisitor {
      override def visitExpression(ref: ScExpression): Unit = {
        ref match {
          case `.run from ZioRunOps`(arg) =>
            buff += (arg.`type`().toOption)
          case `.run from Scala3 Extension`(arg) =>
            buff += (arg.`type`().toOption)
          case _ =>
            super.visitExpression(ref)
        }
      }
    }
    element.accept(visitor)
    buff.toList
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

    totalType
  }

  override val boundMacro: Seq[MacroImpl] = {
    Seq(
      MacroImpl("apply", "zio.direct.defer"),
      MacroImpl("tpe", "zio.direct.defer"),
      MacroImpl("info", "zio.direct.defer"),
      MacroImpl("verbose", "zio.direct.defer"),
      MacroImpl("verboseTree", "zio.direct.defer")
    )
  }
}
