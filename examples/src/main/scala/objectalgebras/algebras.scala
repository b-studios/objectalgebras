package objectalgebras
package examples

object algebras {

  trait Expr[-Expr, +Out] {
    def Add: (Expr, Expr) => Out
    def Lit: Int => Out
  }
  object Expr extends Algebra[Expr]

  trait Stmt[-Stmt, -Expr, +Out] {
  	def If: (Expr, Stmt, Stmt) => Out
  }
  object Stmt extends Algebra1[Stmt]

}
