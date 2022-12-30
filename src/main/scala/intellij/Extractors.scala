package intellij

import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTemplateDefinition, ScTrait}
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.extensions.PsiClassExt
import org.jetbrains.plugins.scala.codeInspection.collections.{isOfClassFrom, _}

import scala.reflect.ClassTag

object Extractors {
  private object Internal {
    sealed abstract class BaseStaticMemberReference(refName: String) {
      protected def matchesRefName(ref: ScReferenceExpression): Boolean =
        if (ref.refName == refName) true
        else
          ref.resolve() match {
            // handles the 'apply' case when called with defer(x)
            case n: ScNamedElement if n.name == refName => true
            case _ => false
          }
    }

    def fqnIfIsOfClassFrom(tpe: ScType, patterns: Seq[String]): Option[String] =
      tpe.tryExtractDesignatorSingleton.extractClass
        .flatMap(Option(_))
        .flatMap(c => Option(c.qualifiedName))
        .find(ScalaNamesUtil.nameFitToPatterns(_, patterns, strict = false))

    sealed abstract class MemberReferenceExtractor[+T <: ScTemplateDefinition : ClassTag] {
      def types: Set[String]

      private def findOverloaded(expr: ScReferenceExpression) =
        expr.multiResolveScala(incomplete = false) match {
          case result if result.isEmpty => None
          case result =>
            result.flatMap(_.fromType).distinct match {
              case Array(tpe) => fqnIfIsOfClassFrom(tpe, types.toSeq)
              case _ => None
            }
        }

      def unapply(ref: ScReferenceExpression): Option[String] =
        ref.resolve() match {
          case null => findOverloaded(ref)
          case t: ScTemplateDefinition if types.contains(t.qualifiedName) => Some(t.qualifiedName)
          case f: ScFunctionDefinition =>
            Option(f.containingClass).collect { case o: T => o.qualifiedName }.filter(types.contains)
          case _ => None
        }
    }

    sealed trait StaticMemberReferenceExtractor extends MemberReferenceExtractor[ScObject]

    object methodExtractors {
      object uncurry1 {
        def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression)] =
          expr match {
            case MethodRepr(_, _, Some(ref), Seq(e)) => Some((ref, e))
            case _ => None
          }
      }
    }
    import methodExtractors._

    sealed abstract class StaticMemberReference(extractor: StaticMemberReferenceExtractor, refName: String)
      extends BaseStaticMemberReference(refName) {

      def unapply(expr: ScExpression): Option[ScExpression] =
        expr match {
          case ref @ ScReferenceExpression(_) if matchesRefName(ref) =>
            ref.smartQualifier match {
              case Some(extractor(fqn)) => Some(expr)
              case _ => None
            }
          case uncurry1(ref, first) if matchesRefName(ref) =>
            ref match {
              case extractor(fqn) => Some(first)
              case _ => None
            }
          case _ => None
        }
    }

    final class DeferStaticMemberReference(refName: String) extends StaticMemberReference(DeferStaticMemberReferenceExtractor, refName)

    object DeferStaticMemberReferenceExtractor extends StaticMemberReferenceExtractor {
      override val types: Set[String] = Set("zio.direct.defer")
    }
  }

  import Internal._

  private val `defer.apply` = new DeferStaticMemberReference("apply")
  private val `defer.tpe` = new DeferStaticMemberReference("tpe")
  private val `defer.info` = new DeferStaticMemberReference("info")
  private val `defer.verbose` = new DeferStaticMemberReference("verbose")
  private val `defer.verboseTree` = new DeferStaticMemberReference("verboseTree")

  object `defer._` {
    def unapply(expr: ScExpression): Option[ScExpression] =
      expr match {
        case `defer.apply`(v) => Some(v)
        case `defer.tpe`(v) => Some(v)
        case `defer.info`(v) => Some(v)
        case `defer.verbose`(v) => Some(v)
        case `defer.verboseTree`(v) => Some(v)
        case _ => None
      }
  }

  // TODO run call from zio.direct package for scala. Need to look into how that's represented
  // Also need to have regular 'run(block)' call
  val `.run from ZioRunOps`: Qualified = invocation("run").from(Seq("zio.direct.ZioRunOps"))
}
