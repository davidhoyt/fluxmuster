package com.github.davidhoyt.fluxmuster

import scala.reflect.runtime.universe._

sealed case class TypeTagTreeNode[T](source: TypeTagTreeSource, typeParameters: Vector[TypeTagTree[_]])(implicit tag: TypeTag[T]) extends TypeTagTree[T] {
  val tpe =
    tag.tpe

  val symbol =
    tpe.typeSymbol

  private[this] val asShortString = {
    val (typeName, typeParamsStart, typeParams, typeParamsEnd) =
      if (typeParameters.isEmpty)
        (symbol.name.decoded, "", "", "")
      else if (tpe <:< typeOf[Product])
        ("", "(", typeParametersAsShortString, ")")
      else
        (symbol.name.decoded, "[", typeParametersAsShortString, "]")
    s"$typeName$typeParamsStart$typeParams$typeParamsEnd"
  }

  private[this] val typeParametersAsShortString = {
    val sb = StringBuilder.newBuilder
    for ((typeParam, idx) <- typeParameters.zipWithIndex) {
      if (idx > 0)
        sb ++= ", "
      sb ++= typeParam.toString
    }
    sb.toString()
  }

  val toShortString =
    asShortString

  override def toString: String =
    asShortString
}

sealed trait RuntimeMirror {
  import scala.reflect.runtime.universe._

  protected def mirror =
    runtimeMirror(Thread.currentThread().getContextClassLoader)
}

/**
 *
 * @param source
 * @param line
 * @param column
 * @param index
 */
sealed case class TypeTagTreeSource(source: String, line: Int, column: Int, index: Int)

sealed trait TypeTagTree[T] {
  val tpe: Type
  val symbol: Symbol
  val typeParameters: Seq[TypeTagTree[_]]
  val source: TypeTagTreeSource
  def toShortString: String
}

sealed case class TypeTagTreeEntry[T](private val root: TypeTagTreeNode[T], source: TypeTagTreeSource) extends TypeTagTree[T] {
  val tpe =
    root.tpe

  val symbol =
    root.symbol

  val typeParameters =
    root.typeParameters

  val toShortString =
    root.toShortString

  override def toString: String =
    s"TypeTagTree(${root.toString}, $source)"
}

object TypeTagTree {
  import scala.language.experimental.macros

  /**
   * The use of `implicit def` is to behave similar to [[scala.reflect.api.TypeTags.TypeTag]] in that the compiler
   * will call our macro to conjure an instance when needed.
   *
   * @tparam T The type whose type tag tree will be generated
   * @return An instance of [[TypeTagTree]]
   */
  implicit def apply[T]: TypeTagTree[T] =
    macro Macros.typeData[T]

  def unapply[T](ttt: TypeTagTree[T]): Option[(Symbol, Type, Seq[TypeTagTree[_]], TypeTagTreeSource)] =
    PartialFunction.condOpt(ttt) {
      case t => (t.symbol, t.tpe, t.typeParameters, t.source)
    }

  private object Macros {
    import scala.reflect.macros._
    import scala.reflect.runtime.universe._

    def typeData[T : c.WeakTypeTag](c: Context): c.Expr[TypeTagTree[T]] = {
      import c.universe._

      val t = c.weakTypeOf[T]
      val symbol = t.typeSymbol.asType
      val pos = c.enclosingPosition

      //Create a TypeTagTreeSource instance
      val file = c.literal(pos.source.toString())
      val line = c.literal(pos.line)
      val column = c.literal(pos.column)
      val index = c.literal(pos.startOrPoint)
      val typeTagTreeSource =
        reify {
          TypeTagTreeSource(file.splice, line.splice, column.splice, index.splice)
        }

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
          newTermName("TypeTagTreeNode")
        )
       */
      val selectTypeTagTreeNode =
        selectWith(typeOf[TypeTagTreeNode[_]].typeSymbol.fullName)

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
      def typeTagTreeEntry(value: c.Type)(typeParameters: List[c.Tree]): c.Tree = {
        Apply(
          TypeApply(
            selectTypeTagTreeNode,
            List(
              //type
              TypeTree(value)
            )
          ),
          List(
            //source
            typeTagTreeSource.tree,
            //typeParameters
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

      def generateTree(tpe: c.Type): c.Tree = {
        val normalized = tpe.normalize
        normalized match {
          case TypeRef(_, sym, args) =>
            typeTagTreeEntry(normalized)(args.map(x => generateTree(x)))
          case _ =>
            EmptyTree
        }
      }

      val root = c.Expr[TypeTagTreeNode[T]](generateTree(t))

      reify {
        TypeTagTreeEntry[T](root.splice, typeTagTreeSource.splice)
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
