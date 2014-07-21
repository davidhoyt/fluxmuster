package com.github.davidhoyt.fluxmuster

import scala.reflect.runtime.universe._

sealed case class TypeDataTree[T](typeParameters: Vector[TypeDataTree[_]])(implicit tag: TypeTag[T]) {
  val tpe = tag.tpe
  val symbol = tpe.typeSymbol

  private[this] lazy val asShortString = {
    val (typeName, typeParamsStart, typeParams, typeParamsEnd) =
      if (typeParameters.isEmpty)
        (symbol.name.decoded, "", "", "")
      else if (tpe <:< typeOf[Product])
        ("", "(", typeParametersAsShortString, ")")
      else
        (symbol.name.decoded, "[", typeParametersAsShortString, "]")
    s"$typeName$typeParamsStart$typeParams$typeParamsEnd"
  }

  lazy val typeParametersAsShortString = {
    val sb = StringBuilder.newBuilder
    for ((typeParam, idx) <- typeParameters.zipWithIndex) {
      if (idx > 0)
        sb ++= ", "
      sb ++= typeParam.toString
    }
    sb.toString()
  }

  def toShortString: String =
    asShortString

  override def toString: String =
    asShortString
}

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

sealed case class TypeData[T](tree: TypeDataTree[T], position: TypePosition) extends RuntimeMirror {
  val tpe =
    tree.tpe

  val symbol =
    tree.symbol

  def toShortString: String =
    s"TypeData(${tree.toShortString}, $position)"

  override def toString: String =
    s"TypeData(${tree.toString}, $position)"
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

      /**
       * Takes a string like "foo.bar.qux" and converts it to:
       *   `Select(Select(Ident(newTermName("foo")), newTermName("bar")), newTermName("qux"))`
       */
      def selectWith(name: String): c.Tree = {
        def recurse(curr: c.Tree, remaining: List[String]): c.Tree = remaining match {
          case head :: tail if tail.nonEmpty =>
            Select(recurse(curr, tail), newTermName(head))
          case head :: _ =>
            Ident(newTermName(head))
        }
        val split = name.split('.').toList.reverse
        recurse(EmptyTree, split)
      }

      /*
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(newTermName("com")
                ),
                newTermName("github")
              ),
              newTermName("davidhoyt")
            ),
            newTermName("fluxmuster")
          ),
          newTermName("TypeDataTree")
        )
       */
      val selectTypeDataTree =
        selectWith(typeOf[TypeDataTree[_]].typeSymbol.fullName)

      /*
        Select(
          Select(
            Ident(newTermName("scala")
          ),
          newTermName("Vector")
        )
       */
      val selectVector =
        selectWith("scala.Vector")

      /**
       * Generates an AST representing the following:
       *   `TypeDataTree[T](Vector(typeParameters))`
       * `T` is the provided type and its implicit type tag is automagically
       * found and created by the compiler.
       *
       * Reify is not eligible because in 2.10 we're unable to splice a List
       */
      def typeDataTree(value: c.Type)(typeParameters: List[c.Tree]): c.Tree = {
        Apply(
          TypeApply(
            selectTypeDataTree,
            List(
              TypeTree(value)
            )
          ),
          List(
            //children
            Apply(
              Select(
                selectVector,
                newTermName("apply")
              ),
              typeParameters
            )
          )
        )
      }

      def toRuntimeUniverse(tpe: c.Type): c.Tree =
        c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, tpe)

      def tryIt(tpe: c.Type): c.Tree = {
        val normalized = tpe.normalize
        normalized match {
          case TypeRef(_, sym, args) =>
            typeDataTree(normalized)(args.map(x => tryIt(x)))
          case _ =>
            EmptyTree
        }
      }

      val tree = c.Expr[TypeDataTree[T]](tryIt(t))
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
        TypeData[T](tree.splice, TypePosition(Option(enclosingClassFullName.splice), file.splice, line.splice, column.splice, index.splice))
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
