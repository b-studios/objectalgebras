package objectalgebras

import org.scalatest._
import shapeless.test._
import syntax.attribute._

class AttributeTests extends FlatSpec with Matchers {

  "An attribute definition" should "come with implicit conversions" in {

    @attr def eval: Int

    val x: Eval = 42

    x.eval should be (42)
  }

  "Accessing a method not defined on an attribute" should "throw a domain specific error" in {
    @attr def eval: Int
    @attr def pretty: String

    import Eval._; import Pretty._;

    val x: Attribute = 42

    illTyped("x.pretty", """Method 'pretty' cannot be invoked:
  'pretty' is not available on the node of type 'objectalgebras.Attribute'.
  Try adding 'Pretty' as dependency to your object algebra.""")
  }

  "Multiple attributes of the same type" should "lead to ambiguous implicits" in {
    @attr def eval: Int
    @attr def count: Int
    import Eval._; import Count._;

    val x: Count = 42
    val y: Eval = 42

    //illTyped("val z: Attribute = 42")
  }
}
