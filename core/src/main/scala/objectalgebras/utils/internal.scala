package objectalgebras
package utils

import scala.reflect.macros.whitebox.Context

trait Internal {

  val c: Context
  import c.universe._
  import scala.reflect.internal


  implicit class SymbolOps(s: Symbol) {
    val intern = s.asInstanceOf[internal.Symbols#Symbol]

    def isCovariant = intern.isCovariant
    def isContravariant = intern.isContravariant
    def isInvariant = ! (isContravariant || isCovariant)
    def isGetter = intern.isGetter
    def tpe = intern.tpe.asInstanceOf[Type]
  }

  implicit class TypeOps(t: Type) {
    val intern = t.asInstanceOf[internal.Types#Type]

    def parents = intern.parents
    def isObjectLike =
      t =:= typeOf[AnyRef] || t =:= typeOf[AnyVal] || t =:= typeOf[Any]
    def abstractMembers: List[Symbol] =
      t.members.toList.filter(_.isAbstract)

    def typeComponents: List[Type] = t.dealias match {
      case RefinedType(parents, _) => parents.flatMap(_.typeComponents)
      case t => List(t)
    }
  }
}
