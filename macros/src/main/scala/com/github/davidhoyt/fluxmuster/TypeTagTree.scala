package com.github.davidhoyt.fluxmuster

import scala.reflect.runtime.universe._

sealed case class TypeTagTreeNode[T](source: TypeTagTreeSource, tpe: Type) extends TypeTagTree[T] {
  import TypeTagTreeNode._

  val symbol =
    tpe.typeSymbol.asType

  val typeParameters =
    evaluate(tpe)

  val isRefined =
    refined(tpe)

  private def evaluate(t: Type) =
    recursivelyMapTypeParameters(t)(x => TypeTagTreeNode(source, x))
}

object TypeTagTreeNode {
  /** Constructs a new instance of [[TypeTagTree]] given the source. */
  def apply[T](source: TypeTagTreeSource)(implicit tag: TypeTag[T]): TypeTagTree[T] =
    TypeTagTreeNode(source, tag.tpe)

  /** Determines if the given `t` [[scala.reflect.runtime.universe.Type]] is a refined type. */
  private def refined(t: Type): Boolean = t match {
    case RefinedType(_, _) => true
    case _ => false
  }

  /**
   * Processes the provided type by recursively processing the type tree in a depth-first
   * manner and produces a tree of [[TypeTagTree]] instances from the discovered type
   * arguments.
   *
   * @param t The [[scala.reflect.runtime.universe.Type]] that will be recursively mapped
   * @param fn A function to apply as types are discovered and which maps from
   *           [[scala.reflect.runtime.universe.Type]] to `A`
   * @tparam A The type that will be produced as a result of applying `fn` to discovered
   *           instances of [[scala.reflect.runtime.universe.Type]]
   * @return
   */
  def recursivelyMapTypeParameters[A](t: Type)(fn: Type => A): Vector[A] = {
    def process(xs: List[Type]): Vector[A] = {
      xs.toVector.map(fn)
    }

    //The most important logic in this file. This describes how to
    //destructure and extract (if any) type parameters for this type.
    //
    //The most important thing to keep in mind is that the "type" is actually
    //an AST where the type can be represented by any of a number of case classes
    //that changes how their type parameters are accessed.
    t match {
      //Anonymous type such as "new Foo[String] {}"
      case RefinedType(TypeRef(_, anyRefSymbol, _) :: typeRefs, _) if anyRefSymbol.fullName == "scala.AnyRef" =>
        process(typeRefs)

      //Refined type such as "Foo with Bar with Baz"
      case RefinedType(typeRefs, _) =>
        process(typeRefs)

      //Existential type such as "Seq[_] forSome ..."
      case ExistentialType(/*quantified*/_, underlying) =>
        process(List(underlying))

      //Standard type
      case TypeRef(_, _, args) =>
        process(args)

      //Unrecognized type
      case _ =>
        throw new IllegalStateException(s"Unable to determine type parameters for $t")
    }
  }
}

/**
 * Provides metadata about where a [[TypeTagTree]] instance was materialized.
 *
 * @param source The source file from which the [[TypeTagTree]] instance was summoned
 * @param line The line in the source
 * @param column The column in the line in the source
 * @param index The number of characters from the beginning of the source
 */
sealed case class TypeTagTreeSource(source: String, line: Int, column: Int, index: Int)

@scala.annotation.implicitNotFound("No TypeTagTree available for ${T}")
sealed trait TypeTagTree[T] {
  /** The [[scala.reflect.runtime.universe.Type]] that this instance represents. */
  val tpe: scala.reflect.runtime.universe.Type

  /** The [[scala.reflect.runtime.universe.TypeSymbol]] that this instance represents. */
  val symbol: scala.reflect.runtime.universe.TypeSymbol

  /** The type parameters, if any, for this [[scala.reflect.runtime.universe.Type]]. */
  val typeParameters: Seq[TypeTagTree[_]]

  /** Provides information about where this [[TypeTagTree]] instance is being used. */
  val source: TypeTagTreeSource

  /** Determine if this [[scala.reflect.runtime.universe.Type]] is considered `refined`. */
  def isRefined: Boolean

  /** Determine if this [[scala.reflect.runtime.universe.Type]] refers to an existential type. */
  lazy val isExistential =
    symbol.isExistential

  /** Provides a string representing the given [[scala.reflect.runtime.universe.Type]]. */
  lazy val toShortString =
    tpe.toString

  /** Provides a string representing this [[TypeTagTree]] instance. */
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
    macro Macros.typeTagTree[T]

  /**
   * Provides an extractor for getting the [[scala.reflect.runtime.universe.TypeSymbol]],
   * the [[scala.reflect.runtime.universe.Type]], the type parameters, and the [[TypeTagTreeSource]]
   * for a [[TypeTagTree]].
   * @param ttt The [[TypeTagTree]] instance that will be extracted
   * @tparam T The [[scala.reflect.runtime.universe.Type]] for the [[TypeTagTree]]
   * @return An extracted view of the provided [[TypeTagTree]] instance
   */
  def unapply[T](ttt: TypeTagTree[T]): Option[(TypeSymbol, Type, Seq[TypeTagTree[_]], TypeTagTreeSource)] =
    PartialFunction.condOpt(ttt) {
      case t => (t.symbol, t.tpe, t.typeParameters, t.source)
    }

  private object Macros {
    import scala.reflect.macros._

    /**
     * Generates a [[TypeTagTree]] instance when applied to a given [[scala.reflect.runtime.universe.Type]].
     */
    def typeTagTree[T : c.WeakTypeTag](c: Context): c.Expr[TypeTagTree[T]] = {
      import c.universe._

      val t = c.weakTypeOf[T]
      val pos = c.enclosingPosition

      //Create a TypeTagTreeSource instance.
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
       *   `TypeTagTreeNode[T](TypeTagTreeSource(...))(&lt;implicitly discovered tag&gt;)`
       * `T` is the provided type and its implicit type tag is automagically
       * found and created by the compiler.
       *
       * Reify is not eligible because in 2.10 we're unable to splice a type parameter
       */
      def typeTagTreeNode(value: c.Type): c.Tree =
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
            typeTagTreeSource.tree
          )
        )

      c.Expr[TypeTagTreeNode[T]](typeTagTreeNode(t))
    }
  }
}
