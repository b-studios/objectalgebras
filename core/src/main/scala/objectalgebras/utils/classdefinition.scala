package objectalgebras
package utils

import scala.reflect.macros.whitebox.Context

trait ClassDefinition { self: Errors =>

  val c: Context
  import c.universe._

  /**
   * Extractor object to parse class definitions and extract
   * most used information.
   */
  case class ClassDefinition (
    name: TypeName,
    tparams: List[TypeDef],
    parents: List[Tree],
    body: List[Tree]
  )
  object ClassDefinition {
    def apply: Tree => ClassDefinition = t => unapply(t) getOrElse
      error(s"Parse Error\n  Cannot parse class definition: ${show(t)}")

    def unapply(t: Tree): Option[ClassDefinition] = t match {
      case ClassDef(mods, name, tparams, Template(parents, self, body)) =>
        Some(ClassDefinition(name, tparams, parents, body))
      case _ => None
    }
  }
}
