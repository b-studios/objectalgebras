package objectalgebras
package utils

import scala.reflect.macros.whitebox.Context

trait MacroApplication { self: Errors =>

  val c: Context
  import c.universe._

  /**
   * Extractor object to parse macro applications and
   * extract macro arguments.
   *
   * Supports multiple value as well as typearguments
   */
  case class MacroApp (
    name: String,
    typeArgs: List[List[Tree]],
    termArgs: List[List[Tree]],
    body: Tree
  )
  object MacroApp {
    def apply: Tree => MacroApp = t => unapply(t) getOrElse
      error(s"Parse Error\n  Cannot parse macro application: ${show(t)}")

    def unapply(t: Tree): Option[MacroApp] = t match {
      case Apply(Select(
        ValueArgs(name, valueArgs, typeArgs),
        TermName("macroTransform")), List(body)) => Some(MacroApp(name, typeArgs, valueArgs, body))
      case _ => None
    }
    object ValueArgs {
      def unapply(t: Tree): Option[(String, List[List[Tree]], List[List[Tree]])] = t match {
        case Select(New(TypeArgs(name, typeArgs)), termNames.CONSTRUCTOR) =>
          Some((name, Nil, typeArgs))
        case Apply(ValueArgs(name, valueArgs, typeArgs), args) =>
          Some((name, valueArgs :+ args, typeArgs))
        case _ => None
      }
    }
    object TypeArgs {
      def unapply(t: Tree): Option[(String, List[List[Tree]])] = t match {
        case AppliedTypeTree(TypeArgs(name, typeArgs), args) =>
          Some((name, typeArgs :+ args))
        case name => Some((name.toString, Nil))
      }
    }
  }
}
