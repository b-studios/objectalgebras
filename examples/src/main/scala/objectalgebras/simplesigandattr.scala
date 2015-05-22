package objectalgebras
package tests

import syntax.attribute._
import syntax.signature._

object signaturesAndAttributes {

  @attr def eval: Int

  @sig trait Expr {
    def Lit: Int
    def Add: (Expr, Expr)
  }

  object evalAlg extends Expr[Eval, Eval] {
    def Lit = identity
    def Add = (l, r) => l.eval + r.eval
  }

  val foldable: Expr.Complete[Eval] = evalAlg


  // nested occurrences
  @sig trait Nested {
    def List: List[Nested]
    def Option: Option[Nested]
  }


}
