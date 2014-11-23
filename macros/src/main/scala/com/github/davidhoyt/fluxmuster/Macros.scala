package com.github.davidhoyt.fluxmuster

object Macros {
  import scala.reflect.runtime.universe._
  import scala.reflect.macros._

  import scala.language.experimental.macros

  def nameOf[T]: String =
    macro Run.nameOf[T]

  def simpleNameOf[T]: String =
    macro Run.simpleNameOf[T]

  def isIdentity[A, B](fn: A => B): Boolean =
    macro Run.isIdentity[A, B]

  private object Run {
    import scala.reflect.macros._

    def nameOf[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[String] = {
      import c.universe._

      val t = c.weakTypeOf[T]
      val symbol = t.typeSymbol
      val name = symbol.fullName

      c.Expr[String] {
        q"$name"
      }
    }

    def simpleNameOf[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[String] = {
      import c.universe._

      val t = c.weakTypeOf[T]
      val symbol = t.typeSymbol
      val name = symbol.name.decodedName.toString

      c.Expr[String] {
        q"$name"
      }
    }

    def isIdentity[A : c.WeakTypeTag, B : c.WeakTypeTag](c: blackbox.Context)(fn: c.Expr[A => B]): c.Expr[Boolean] = {
      import c.universe._

      val isIdentity = fn.tree match {
        //case q"""{ (x: Int) => scala.this.Predef.identity[Int](x) }""" =>
        case Block(_, Function(params, Apply(TypeApply(select @ Select(_, _), _), _))) =>
          select.symbol.isMethod && select.symbol.fullName == "scala.Predef.identity"
        case _ =>
          false
      }

      c.Expr[Boolean] {
        q"$isIdentity"
      }
    }
  }
}
