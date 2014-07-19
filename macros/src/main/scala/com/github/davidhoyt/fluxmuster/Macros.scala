package com.github.davidhoyt.fluxmuster

sealed trait RuntimeMirror {
  import scala.reflect.runtime.universe._

  protected def mirror =
    runtimeMirror(Thread.currentThread().getContextClassLoader)
}

sealed case class TypePosition(enclosingClassFullName: Option[String], source: String, line: Int, column: Int, index: Int) extends RuntimeMirror {
  val enclosingClassSymbol =
    enclosingClassFullName map mirror.staticClass

  val enclosingClassType =
    enclosingClassSymbol map (_.toType)
}

sealed case class TypeData[T](fullName: String, position: TypePosition) extends RuntimeMirror {
  val symbol =
    mirror.staticClass(fullName).asType

  val tpe =
    symbol.toType
}

object TypeData {
  import scala.language.experimental.macros

  implicit def apply[T]: TypeData[T] =
    macro Macros.typeData[T]

  private object Macros {
    import scala.reflect.macros._
    import scala.reflect.runtime.universe._

    def typeData[T : c.WeakTypeTag](c: Context): c.Expr[TypeData[T]] = {
      import c.universe._

      val t = c.weakTypeOf[T]
      val symbol = t.typeSymbol.asType
      val pos = c.enclosingPosition

      val name = c.literal(symbol.fullName)
      val file = c.literal(pos.source.toString())
      val line = c.literal(pos.line)
      val column = c.literal(pos.column)
      val index = c.literal(pos.startOrPoint)
      val enclosingClassFullName =
        if (c.enclosingClass.isEmpty)
          c.literalNull
        else
          c.literal(c.enclosingClass.symbol.fullName)

      reify {
        TypeData[T](name.splice, TypePosition(Option(enclosingClassFullName.splice), file.splice, line.splice, column.splice, index.splice))
      }
    }
  }
}

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
