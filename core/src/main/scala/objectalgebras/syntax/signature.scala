package objectalgebras
package syntax

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context
import scala.collection.mutable

import utils.Utils

package object signature {

  /**
   * Not yet supported:
   * - families of signatures: `@sig(Expr) trait Stmt {...}`
   * - define superclass to extend `@sig trait MulExpr extends Expr {}`
   * - second input format `def Add(lhs: Expr, rhs: Expr)`
   */
  final class sig extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro SigAnnotationMacro.apply
  }

  class SigAnnotationMacro(val c: Context) extends MacroApplication with Errors {
    import c.universe._
    import c.Expr

    def apply(annottees: Expr[Any]*): Expr[Any] = {
      //val MacroApp(_, _, List(members), _) = MacroApp(c.macroApplication)
      val m = MacroApp(c.macroApplication)
      println(m)

      val (sigName, _, defs) = processInput(annottees.head.tree)
      Expr(Sig(Names(sigName), defs).gen)
    }

    // An extractor object for constructor definitions
    object Constructor {
      def unapply(t: Tree): Option[(TermName, List[Tree])] = t match {
        case q"""def $name: (..$args)""" => Some((name, args))
        case q"""def $name: $arg""" => Some((name, List(arg)))
        case _ => None
      }
    }

    /**
     * Parses the input and returns tuple (name, baseClasses, definitions)
     */
    def processInput(t: Tree): (TypeName, List[Tree], List[Tree]) = t match {
      case ClassDef(mods, name, _, Template(sup, self, defs)) => (name, sup, defs)
      case t =>
        c.abort(c.enclosingPosition,
          s"""Wrong shape for an signature definition
             |  Expected something like:
             |
             |  @sig trait Expr {
             |    def Lit: Int
             |    def Add: (Expr, Expr)
             |  }""".stripMargin)
    }

    case class Names(in: TypeName) {
      // strings
      val name = in.toString

      // terms
      val companion = TermName(name)

      // types
      val sig = in
      val out = TypeName("Out")
    }

    case class Sig(names: Names, defs: List[Tree]) {

      def genConstructor: Tree => Tree = {
        case Constructor(name, args) => {q"""
          def $name: (..$args) => ${names.out}
        """}
      }

      lazy val genSignature = q"""
        trait ${names.sig}[-${names.sig}, +${names.out}] extends Signature {
          ..${defs map genConstructor}
        }
      """

      lazy val genCompanion = q"""
        object ${names.companion} extends Algebra[${names.sig}] {
          // TODO implement combinators here
        }
      """

      lazy val gen = q"""$genSignature; $genCompanion"""
    }
  }
}
