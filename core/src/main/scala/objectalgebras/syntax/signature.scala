package objectalgebras
package syntax

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable

import utils.Utils

package object signature {

  /**
   * Not yet supported:
   * - define superclass to extend `@sig trait MulExpr extends Expr {}`
   * - second input format `def Add(lhs: Expr, rhs: Expr)`
   */
  final class sig extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro SigAnnotationMacro.apply
  }

  class SigAnnotationMacro(val c: Context) extends Utils {
    import c.universe._
    import c.Expr

    def apply(annottees: Expr[Any]*): Expr[Any] = {

      val targs = MacroApp(c.macroApplication) match {
        case MacroApp(_, Nil, _, _) => Nil
        case MacroApp(_, List(targs), _, _) => targs map {
          case Ident(t: TypeName) => t
        }
      }

      val cls = ClassDefinition unapply annottees.head.tree getOrElse
        error(s"""Wrong shape for an signature definition
         |  Expected something like:
         |
         |  @sig trait Expr {
         |    def Lit: Int
         |    def Add: (Expr, Expr)
         |  }""".stripMargin)

      Expr(Sig(Names(cls.name), cls.body, targs).gen)
    }

    // An extractor object for constructor definitions
    object Constructor {
      def unapply(t: Tree): Option[(TermName, List[Tree])] = t match {
        case q"""def $name: (..$args)""" => Some((name, args))
        case q"""def $name: $arg""" => Some((name, List(arg)))
        case _ => None
      }
    }

    case class Names(in: TypeName) {
      // strings
      val name = in.toString

      // terms
      val companion = TermName(name)

      // types
      val sig = in
      val out = TypeName("Out")
    }

    case class Sig(names: Names, defs: List[Tree], tparams: List[TypeName]) {

      import Flag._

      def genConstructor: Tree => Tree = {
        case Constructor(name, args) => {q"""
          def $name: (..$args) => ${names.out}
        """}
      }

      // x => tq"-$x"
      lazy val genTypeParams =
        ((names.sig +: tparams) map (x =>
          TypeDef(
            Modifiers(PARAM | CONTRAVARIANT),
            x,
            Nil, TypeBoundsTree(TypeTree(), TypeTree())))
        ) :+ TypeDef(
          Modifiers(PARAM | COVARIANT),
          names.out,
          Nil, TypeBoundsTree(TypeTree(), TypeTree()))

      lazy val genSignature = q"""
        trait ${names.sig}[..$genTypeParams] extends Signature {
          ..${defs map genConstructor}
        }
      """

      lazy val genAlgebraName = tparams.size match {
        case 0 => TypeName("Algebra")
        case n => TypeName("Algebra" + n)
      }

      lazy val genCompanion = q"""
        object ${names.companion} extends $genAlgebraName[${names.sig}] {
          // TODO implement combinators here

          import scala.language.experimental.macros
          import scala.annotation.StaticAnnotation
          import scala.reflect.macros.whitebox.Context

          final class alg extends StaticAnnotation {
            def macroTransform(annottees: Any*): Any = macro AlgAnnotationMacro.apply
          }
          class AlgAnnotationMacro(val c: Context) extends AlgebraMacro {
            import c.universe.TypeName
            val sigName = TypeName(${names.sig.toString})
            val dependencies = List(..${tparams map (_.toString)}).map {
              n => TypeName(n)
            }
          }
        }
      """

      lazy val gen = q"""$genSignature; $genCompanion"""
    }
  }


  trait AlgebraMacro extends Utils {
    val c: Context
    import c.universe._

    // This is the information lifted from the previous macro run:
    def sigName: TypeName
    def dependencies: List[TypeName]

    // TODO add support for anonymous algebras
    //   i.e. `@Expr.alg ... object Foo` (no trait is defined)
    def apply(annottees: c.Expr[Any]*): c.Expr[Any] = {

      val m = MacroApp(c.macroApplication)

      val out   = TypeName("Out")
      val child = sigName

      val allKeys = dependencies ++ List(sigName, out)

      // initialize all with Any:
      val depMap = mutable.Map[TypeName, List[TypeName]](allKeys map ((_, Nil)): _*)

      def setType(key: TypeName, t: TypeName) =
        depMap.update(key, t :: Nil)

      def addType(key: TypeName, t: TypeName) =
        depMap.update(key, depMap(key) :+ t)

      m.typeArgs match {
        case List(List(Ident(res: TypeName))) =>
          setType(out, res)
          setType(child, res)

        case List(
          List(Ident(res: TypeName)),
          eqs
        ) =>
          setType(out, res)
          setType(child, res)
          eqs foreach {
            case tq"${Ident(k: TypeName)} := ${Ident(v: TypeName)}" =>
              setType(k, v)
            case tq"${Ident(k: TypeName)} += ${Ident(v: TypeName)}" =>
              addType(k, v)
          }
      }

      val types = allKeys map depMap map {
        case ts => ts.foldLeft[Tree](tq"Any") {
          case (l, r) => tq"$l with $r"
        }
      }

      // XXX have better error message here
      val cls = ClassDefinition unapply annottees.head.tree getOrElse
        error(s"""Wrong shape for an algebra definition""")

      val body = cls.body.filterNot {
        case DefDef(_, TermName("$init$"), _, _,_,_) => true
        case _ => false
      }

      val companion = TermName(cls.name.toString)

      c.Expr(q"""
        trait ${cls.name} extends $sigName[..$types] { ..$body }

        // XXX not sure whether we should always generate this.
        // maybe typecheck and see, whether all methods are defined
        object $companion extends ${cls.name}
      """)
    }
  }
}
