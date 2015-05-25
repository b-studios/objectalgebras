package objectalgebras
package examples

import syntax.signature._

object signatures {

  @sig trait Expr {
    def Lit: Int
    def Add: (Expr, Expr)
  }

  object eval extends Expr[Int, Int] {
    def Lit = identity
    def Add = _ + _
  }

  @sig trait Nested {
    def List: List[Nested]
    def Option: Option[Nested]
  }


  val foldable: Expr.Complete[Int] = eval

}
