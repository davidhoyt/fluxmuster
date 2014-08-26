package com.github.davidhoyt.fluxmuster

object Macros {
  import scala.reflect.runtime.universe._
  import scala.reflect.macros._

  import scala.language.experimental.macros

  def nameOf[T]: String =
    macro Run.nameOf[T]

  def simpleNameOf[T]: String =
    macro Run.simpleNameOf[T]

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
  }
}
