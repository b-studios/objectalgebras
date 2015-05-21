package objectalgebras
package syntax

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context


package object attribute extends AttributeFallbacks {

  class attr extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro attrImpl
  }

  def attrImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val (attrName, attrType) = annottees.head.tree match {
      case q"def $name: $tpe" => (name, tpe)
      case q"def $name[$tArgs]: $tpe" =>
        c.abort(c.enclosingPosition, "parametrized attributes are not yet supported")
      case t =>
        c.abort(c.enclosingPosition, s"Wrong shape for an attribute definition\n  Expected something like: `@attr def foo: Bar`")
    }

    object names {
      val str = attrName.toString
      val cap = str.capitalize
      val cls = TypeName(cap)
      val cmp = TermName(cap)
      val prm = TermName(c.freshName("x"))
      val low = TypeName(s"${cap}Fallbacks")
      val fallback = TypeName(s"${cap}AccessFallback")
      val err = TypeName(s"${cap}AccessError")
    }

    val memberNotFoundErrorMessage =
      s"""|Method '${names.str}' cannot be invoked:
          |  '${names.str}' is not available on the node of type '$${E}'.
          |  Try adding '${names.cap}' as dependency to your object algebra.""".stripMargin

    val res = q"""trait ${names.cls} extends Attribute {
      def $attrName: $attrType
    }
    object ${names.cmp} extends ${names.low} {
      implicit def $attrName(${names.prm} : $attrType): ${names.cls} = new ${names.cls} {
        def $attrName: $attrType = ${names.prm}
      }
    }
    trait ${names.low} {
      @annotation.implicitNotFound(msg = $memberNotFoundErrorMessage)
      sealed trait ${names.err}[E]

      implicit class ${names.fallback}[A <: Attribute](a: A) {
        def $attrName(implicit err: ${names.err}[A]): $attrType = ???
      }

    }
    """

    c.Expr(res)
  }

}

/**
 * Implicit conversions that are triggered solely to fail
 * in order to provide better error messages.
 */
trait AttributeFallbacks {

  @implicitNotFound(msg = "Cannot automatically wrap `${A}` in an attribute definition when merging with `${B}`. Probably there are ambiguous implicit conversions available or the attribute definition is not in scope. Try explicitly invoking the conversion to the appropriate attribute.")
  sealed trait AutoLiftingFailed[A, B]

  implicit class CatchAttributeOps[A](a: A) {
    def :&:[B](b: B)(implicit err: AutoLiftingFailed[A, B]) = ???
  }
}
