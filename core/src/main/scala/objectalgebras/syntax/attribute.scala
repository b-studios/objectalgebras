package objectalgebras
package syntax

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

package object attribute extends AttributeFallbacks {

  class attr extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro AttrAnnotationMacro.apply
  }

  class AttrAnnotationMacro(val c: Context) {
    import c.universe._
    import c.Expr

    def apply(annottees: Expr[Any]*): Expr[Any] = {
      val (name, tpe) = processInput(annottees.head.tree)
      Expr(Attr(Names(name), tpe).gen)
    }

    def processInput(t: Tree): (TermName, Tree) = t match {
      case q"def $name: $tpe" => (name, tpe)
      case q"def $name[$tArgs]: $tpe" =>
        c.abort(c.enclosingPosition,
          "parametrized attributes are not yet supported")
      case t =>
        c.abort(c.enclosingPosition,
          s"""Wrong shape for an attribute definition
             |  Expected something like: `@attr def foo: Bar`""".stripMargin)
    }

    case class Names(in: TermName) {
      // strings
      val name = in.toString
      val cap  = name.capitalize

      // terms
      val attr      = in
      val companion = TermName(cap)
      val param     = TermName(c.freshName("x"))

      // types
      val cls       = TypeName(cap)
      val low       = TypeName(s"${cap}Fallbacks")
      val fallback  = TypeName(s"${cap}AccessFallback")
      val err       = TypeName(s"${cap}AccessError")
    }

    case class Attr(names: Names, resultType: Tree) {

      lazy val memberNotFoundErrorMessage =
        s"""|Method '${names.name}' cannot be invoked:
            |  '${names.name}' is not available on the node of type '$${E}'.
            |  Try adding '${names.cap}' as dependency to your object algebra.""".stripMargin


      lazy val genTrait = q"""
        trait ${names.cls} extends Attribute {
          def ${names.attr}: $resultType
        }
      """

      lazy val genCompanion = q"""
        object ${names.companion} extends ${names.low} {
          // this really should be a macro to avoid forcing the evaluation!
          implicit def ${names.attr}(${names.param} : $resultType): ${names.cls}
            = new ${names.cls} {
              def ${names.attr}: $resultType = ${names.param}
            }
        }
      """

      lazy val genLowPrioImplicits = q"""
        trait ${names.low} {
          @annotation.implicitNotFound(msg = $memberNotFoundErrorMessage)
          sealed trait ${names.err}[E]

          implicit class ${names.fallback}[A <: Attribute](a: A) {
            def ${names.attr}(implicit err: ${names.err}[A]): $resultType = ???
          }
        }
      """

      lazy val gen = q"""$genTrait; $genCompanion; $genLowPrioImplicits"""
    }
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
