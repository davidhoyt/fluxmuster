package com.github.davidhoyt.fluxmuster

object Macros {
  import scala.language.experimental.macros
  import scala.reflect.runtime.universe._

  def nameOf[T]: String =
    macro Run.nameOf[T]

  private object Run {
    import scala.reflect.macros._

    def nameOf[T : c.WeakTypeTag](c: Context): c.Expr[String] = {
      import c.universe._

      val t = c.weakTypeOf[T]
      val symbol = t.typeSymbol

      c.literal(symbol.fullName)
    }
  }
}
