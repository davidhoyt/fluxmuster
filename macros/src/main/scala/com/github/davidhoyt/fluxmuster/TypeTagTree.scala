package com.github.davidhoyt.fluxmuster

import scala.reflect.runtime.universe._

sealed case class TypeTagTreeNode[T](source: TypeTagTreeSource, tpe: Type) extends TypeTagTree[T] {
  val symbol =
    tpe.typeSymbol.asType

  def evaluate(t: Type): Vector[TypeTagTree[_]] = {
    def process(xs: List[Type]): Vector[TypeTagTree[_]] = {
      xs.toVector.map(x => TypeTagTreeNode(source, x))
    }

    //println(showRaw(tpe))

    t match {
      //Anonymous type
      case RefinedType(TypeRef(_, anyRefSymbol, _) :: typeRefs, _) if anyRefSymbol.fullName == "scala.AnyRef" =>
        process(typeRefs)

      //Refined type
      case RefinedType(typeRefs, _) =>
        process(typeRefs)

      //TODO: Why doesn't this work??
      //Existential type
      case ExistentialType(/*quantified*/_, underlying) => // ExistentialType(_, TypeRef(_, _, args)) =>
        process(List(underlying))
        //process(quantified.map(_.asType.toType))

      //Standard type
      case TypeRef(_, _, args) =>
        process(args)

      //Unrecognized type
      case _ =>
        throw new IllegalStateException(s"Unable to determine type parameters for $tpe")
    }
  }

  val typeParameters =
    evaluate(tpe)
}

object TypeTagTreeNode {
  def apply[T](source: TypeTagTreeSource)(implicit tag: TypeTag[T]): TypeTagTree[T] =
    TypeTagTreeNode(source, tag.tpe)
}

/**
 *
 * @param source
 * @param line
 * @param column
 * @param index
 */
sealed case class TypeTagTreeSource(source: String, line: Int, column: Int, index: Int)

@scala.annotation.implicitNotFound("No TypeTagTree available for ${T}")
sealed trait TypeTagTree[T] {
  val tpe: scala.reflect.runtime.universe.Type
  val symbol: scala.reflect.runtime.universe.TypeSymbol
  val typeParameters: Seq[TypeTagTree[_]]
  val source: TypeTagTreeSource

  lazy val toShortString = tpe.toString
  override def toString: String =
    s"TypeTagTree($toShortString, $source)"
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

    def typeData[T : c.WeakTypeTag](c: Context): c.Expr[TypeTagTree[T]] = {
      import c.universe._

      val t = c.weakTypeOf[T]
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

      /**
       * Generates an AST representing the following:
       *   `TypeTagTreeNode[T](TypeTagTreeSource(...), Vector(typeParameters))`
       * `T` is the provided type and its implicit type tag is automagically
       * found and created by the compiler.
       *
       * Reify is not eligible because in 2.10 we're unable to splice a List
       */
      def typeTagTreeNode(value: c.Type)/*(typeParameters: List[c.Tree])*/: c.Tree =
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
            typeTagTreeSource.tree //,
            ////typeParameters
            //Apply(
            //  Select(
            //    selectVector,
            //    newTermName("apply")
            //  ),
            //  typeParameters
            //)
          )
        )

      c.Expr[TypeTagTreeNode[T]](typeTagTreeNode(t))
    }
  }
}
