package objectalgebras
package examples
package test

// some signatures defined in example project
import signatures._
import syntax.attribute._

import org.scalatest._

class AlgebraTests extends FlatSpec with Matchers  {

  @attr def eval: Int
  @attr def pretty: String

  //import Eval._

  // simple evaluating algebra
  @Expr.alg [Int] trait Evaluator {
    def Lit = n => n
    def Add = (l, r) => l + r
  }

  implicitly[Evaluator <:< Expr[Int, Int]]

  @Expr.alg [Eval] trait Evaluator2 {
    def Lit = n => n
    def Add = (l, r) => l.eval + r.eval
  }

  // defines a pretty printing algebra
  // that requires Eval additionally for the
  // recursive occurrences.
  @Expr.alg [Pretty] [Expr += Eval, Out := Pretty]
  trait Foo {
    def Lit = n => n.toString
    def Add = (l, r) =>
      s"${l.eval} ${r.eval} | ${l.pretty} + ${r.pretty}"
  }

  implicitly[Foo <:< Expr[Pretty with Eval, Pretty]]

  // not supported yet:

  // @Expr.alg [
  //   Expr := Eval with Pretty,
  //   Out  := Pretty]
  // trait Foo2

  def exp[T](alg: Expr.Complete[T]): T = {
    import alg._
    Add(Lit(1), Add(Lit(2), Lit(3)))
  }

  exp(Evaluator) should be (6)
  exp(Evaluator2).eval should be (6)
}

