package objectalgebras
package examples

import syntax.attribute._

object attributes {

  @attr def eval: Int
  @attr def pretty: String
  @attr def prettypretty: String
  @attr def fun: Int => String

  import Eval._; import Pretty._; import Fun._

  val x: Eval = 42

  val y: Attribute = ???

  //y.pretty

  val f: Fun = fun(n => "45")
  f.fun(42)

}
