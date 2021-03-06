package objectalgebras
package utils

import scala.reflect.macros.whitebox.Context

trait Errors {

  val c: Context

  def error(msg: String) = c.abort(c.enclosingPosition, msg)
}
