package intellij

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.{Qualified, invocation}
import org.jetbrains.plugins.scala.lang.macros.evaluator.{MacroContext, MacroImpl, ScalaMacroTypeable}
import org.jetbrains.plugins.scala.lang.psi.api.{ScalaPsiElement, ScalaRecursiveElementVisitor}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScExpression, ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.impl.expr.ScMethodCallImpl
import org.jetbrains.plugins.scala.lang.psi.types.{NamedType, ScType, ScalaType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, StdTypes, ValueType}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.NonValueType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import org.jetbrains.plugins.scala.lang.psi.types.api.{Any, FunctionType, Nothing}


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

  case class RunCall private (element: PsiElement) {

  }

  val `.run from ZioRunOps`: Qualified = invocation("run").from(Seq("zio.direct.ZioRunOps"))

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

  // TODO For defer in defer, assume that the outer one has run first and it's type is correct so just write
  //      the output type of inner defers into the outer context
  def findFirstDeferCall(element: PsiElement): Option[ScMethodCall] =
    element match {
      case m: ScMethodCall if (m.getInvokedExpr.getText == "defer") => Some(m)
      case _ =>
        element.getChildren.find(findFirstDeferCall(_).isDefined).map(_.asInstanceOf[ScMethodCall])
    }

  // TODO do we need to skip runs in nested defers? check that we don't go into them???
  //      need to see an example of that
  def findRunTypes(element: PsiElement): List[TypeResult] =
    element match {
      case `.run from ZioRunOps`(arg) => List(arg.`type`())
      case _ =>
        element.getChildren.flatMap(findRunTypes(_)).toList
    }

  override def checkMacro(macros: ScFunction, context: MacroContext): Option[ScType] = {
    //context.place.
//    val visitor = new ScalaRecursiveElementVisitor {
//      override def visitScalaElement(ref: ScalaPsiElement): Unit = {
//        ref match {
//          //case x @ RunCall(tpe) => println(s"Found expression of ${tpe} : ${x}")
//          case x @ `.run from ZioRunOps`(method, args) =>
//            println(x)
//          case _ =>
//        }
//      }
//    }
//    context.place.accept(visitor)

    macros.isInScala3Module



    def getBodyAndBodyType(callOpt: Option[ScMethodCall]) = {
      for {
        call <- callOpt
        // getFirstChild does strange things when it's it's a primitive e.g. an IntegerLiteral so not using it
        // get the defer body
        // TODO doesn't seem to be working right, need to have a closer look
        argElement <- call.args.exprs.headOption
        arg <-
          argElement match {
            case expr: ScExpression => Some(expr)
            case _ => None
          }
        tpe <- arg.`type`().toOption
      } yield (tpe, arg)
    }

    // TODO need to check invocatios of defer.info etc... and also when a withAbstractError is used to get the least upper bound for errors
    //      if this override is specified for Scala 3
    val firstdeferCallOpt = findFirstDeferCall(context.place)
    val firstdeferCallOptDetails = getBodyAndBodyType(firstdeferCallOpt)
    implicit val ctx =
      ZioDirectContext(
        if (macros.isInScala3Module) UnionType.Or else UnionType.LUB,
        context
      )

    val totalType =
      if (firstdeferCallOptDetails.isEmpty)
        None
      else {
        val (bodyType, body) = firstdeferCallOptDetails.get

        // TODO can check if it's scala 3 directly on the context.place, there should be a helper method for that
        // todo need to check if any of these are None and return Any with early exit
        val runTypesOpt = {
          // NOTE all of this is assuming that the environment and error parameters are associative
          // up to down (in multiple lines of code) and left to right (in the same line of code)
          // based on the flatMap-chain structure produced by zio-direct I believe this is the lawful behavior
          findRunTypes(body).map(r => r match {
              case Left(_) => None
              case Right(tpe) => Some(tpe)
            }
          )
        }

        val totalMod =
          if (runTypesOpt.exists(_.isEmpty)) {
            None
          } else {
            val runTypesModOpt = runTypesOpt.map(_.get).map(ZioMod.unapply(_))
            if (runTypesModOpt.nonEmpty)
              runTypesModOpt.reduce((a, b) => {
                // if both are defined, compose them, the composition can also be none
                for {
                  aVal <- a
                  bVal <- b
                  result <- aVal.compseWith(bVal)
                } yield result
              })
            else
              Some(ZioMod.default)
          }
        totalMod.flatMap(_.toZio(bodyType))
      }




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
