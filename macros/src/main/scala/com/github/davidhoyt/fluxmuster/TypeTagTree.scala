package com.github.davidhoyt.fluxmuster

import scala.reflect.runtime.universe._

sealed case class TypeTagTreeNode[T](source: TypeTagTreeSource, typeParameters: Vector[TypeTagTree[_]])(implicit tag: TypeTag[T]) extends TypeTagTree[T] {
  val tpe =
    tag.tpe.normalize

  val symbol =
    tpe.typeSymbol.asType

  lazy val isStructuralType =
    "<refinement>" == symbol.name.decoded

  lazy val isExistential =
    symbol.isExistential

  lazy val isTuple =
    tpe <:< typeOf[Product] && symbol.isClass && symbol.fullName.startsWith("scala.Tuple")

  lazy val name =
    if (isExistential)
      "_"
    else if (isStructuralType)
      tpe.toString
    else
      symbol.name.decoded

  //TODO: Handle symbolic 2-param types. e.g.: ::[A, B] should be A :: B
  private[this] lazy val asShortString = {
    val (typeName, typeParamsStart, typeParams, typeParamsEnd) =
      if (typeParameters.isEmpty)
        (name, "", "", "")
      else if (isTuple)
        ("", "(", typeParametersAsShortString, ")")
      else
        (name, "[", typeParametersAsShortString, "]")
    s"$typeName$typeParamsStart$typeParams$typeParamsEnd"
  }

  private[this] lazy val typeParametersAsShortString = {
    val sb = StringBuilder.newBuilder
    for ((typeParam, idx) <- typeParameters.zipWithIndex) {
      if (idx > 0)
        sb ++= ", "
      sb ++= typeParam.toShortString
    }
    sb.toString()
  }

  lazy val toShortString =
    asShortString

  override def toString: String =
    s"TypeTagTree($asShortString, $source)"
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
  val symbol: TypeSymbol
  val typeParameters: Seq[TypeTagTree[_]]
  val source: TypeTagTreeSource
  def toShortString: String
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
        selectWith(typeOf[Vector[_]].typeSymbol.fullName)

      /**
       * Generates an AST representing the following:
       *   `TypeTagTreeNode[T](TypeTagTreeSource(...), Vector(typeParameters))`
       * `T` is the provided type and its implicit type tag is automagically
       * found and created by the compiler.
       *
       * Reify is not eligible because in 2.10 we're unable to splice a List
       */
      def typeTagTreeNode(value: c.Type)(typeParameters: List[c.Tree]): c.Tree = {
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

      def writeToFile(p: String, s: String): Unit = {
        val pw = new java.io.PrintWriter(new java.io.File(p))
        try pw.write(s) finally pw.close()
      }

      def generateTree(tpe: c.Type): c.Tree = {
        //val normalized = tpe.normalize
        tpe match {

          //Structural type
          case RefinedType(List(TypeRef(_, _, args)), _) =>
            typeTagTreeNode(tpe)(List.empty)

          //Existential type
          case ExistentialType(_, tref @ TypeRef(_, _, args)) =>
            typeTagTreeNode(tpe)(args.map(x => generateTree(x)))

          //Standard type
          case TypeRef(_, _, args) =>
            typeTagTreeNode(tpe)(args.map(x => generateTree(x)))

          //Unrecognized type
          case _ =>
            EmptyTree
        }
      }

      val root =
        c.Expr[TypeTagTreeNode[T]](generateTree(t))

      //try {
      //  writeToFile("/tmp/generateTree", showRaw(t))
      //  writeToFile("/tmp/macro", showRaw(root.tree))
      //} catch {
      //  case t: Throwable =>
      //    writeToFile("/tmp/macro", t.toString)
      //    throw t
      //}

      if (!root.tree.isEmpty)
        root
      else
        c.abort(pos, s"Unable to generate TypeTagTree for ${symbol.fullName}")
    }
  }
}
