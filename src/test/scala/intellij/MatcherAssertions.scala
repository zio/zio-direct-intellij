package intellij

//import org.jetbrains.plugins.scala.annotator.Message
//import org.jetbrains.plugins.scala.base.FailableTest
import org.junit.Assert

import scala.reflect.ClassTag

trait MatcherAssertions extends FailableTest {

  def assertNothing[T](actual: Option[T]): Unit =
    assertMatches(actual) {
      case Nil =>
    }

  def assertMatches[T](actual: Option[T])(pattern: PartialFunction[T, Unit]): Unit =
    actual match {
      case Some(value) =>
        def message = if (shouldPass) {
          val actualValueFancy = value match {
            case seq: Seq[_] => seq.mkString(s"${seq.getClass.getSimpleName}(\n  ", ",\n  ", "\n)")
            case v                  => v.toString
          }
          "actual: " + actualValueFancy
        } else {
          failingPassed
        }
        Assert.assertTrue(message, shouldPass == pattern.isDefinedAt(value))
      case None => Assert.assertFalse(shouldPass)
    }

  def assertNothing[T](actual: T): Unit =
    assertNothing(Some(actual))

  def assertMatches[T](actual: T)(pattern: PartialFunction[T, Unit]): Unit =
    assertMatches(Some(actual))(pattern)

  def assertMessages(actual: List[Message])(expected: Message*): Unit =
    assertEqualsFailable(expected.mkString("\n"), actual.mkString("\n"))

  def assertMessagesSorted(actual: List[Message])(expected: Message*): Unit =
    assertMessages(actual.sorted)(expected.sorted: _*)

  def assertIsA[T](obj: Object)(implicit classTag: ClassTag[T]): T =
    if (classTag.runtimeClass.isInstance(obj)) {
      obj.asInstanceOf[T]
    } else {
      Assert.fail(s"wrong object class\nexpected ${classTag.runtimeClass.getName}\nactual:${obj.getClass.getName}").asInstanceOf[Nothing]
    }
}

object MatcherAssertions extends MatcherAssertions

import org.junit.Assert

trait FailableTest {

  /**
   * A hook to allow tests that are currently failing to pass when they fail and vice versa.
   * @return
   */
  protected def shouldPass: Boolean = true

  protected def assertEqualsFailable(expected: AnyRef, actual: AnyRef): Unit = {
    if (shouldPass) Assert.assertEquals(expected, actual)
    else Assert.assertNotEquals(expected, actual)
  }

  protected val failingPassed: String = "Test has passed, but was supposed to fail"
}


import scala.math.Ordered.orderingToOrdered

sealed abstract class Message extends Ordered[Message] {
  def element: String
  def message: String

  override def compare(that: Message): Int =
    (this.element, this.message) compare (that.element, that.message)
}
// TODO: move it to Message companion object
case class Info(override val element: String, override val message: String) extends Message
case class Warning(override val element: String, override val message: String) extends Message
case class Error(override val element: String, override val message: String) extends Message
case class Hint(override val element: String, text: String, override val message: String = "", offsetDelta: Int = 0) extends Message