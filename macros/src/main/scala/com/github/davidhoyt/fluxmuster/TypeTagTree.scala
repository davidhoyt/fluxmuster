package com.github.davidhoyt.fluxmuster

import scala.reflect.runtime.universe._

sealed case class TypeTagTreeNode[T](source: TypeTagTreeSource, tpe: Type) extends TypeTagTree[T] {
  import TypeTagTreeNode._

  val symbol =
    tpe.typeSymbol.asType

  val typeArguments =
    evaluate(tpe)

  val isRefined =
    refined(tpe)

  private def evaluate(t: Type) =
    recursivelyMapTypeArguments(t)(x => TypeTagTreeNode(source, x))
}

object TypeTagTreeNode {
  private val AnyRefType = typeOf[AnyRef]

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
  def recursivelyMapTypeArguments[A](t: Type)(fn: Type => A): Vector[A] = {
    def process(xs: List[Type]): Vector[A] = {
      xs.toVector.map(fn)
    }

    //The most important logic in this file. This describes how to
    //destructure and extract (if any) type arguments for this type.
    //
    //The most important thing to keep in mind is that the "type" is actually
    //an AST where the type can be represented by any of a number of case classes
    //that changes how their type arguments are accessed.
    t match {
      //Anonymous type such as "new Foo[String] {}"
      case RefinedType((possiblyAnyRef @ TypeRef(_, _, _)) :: typeRefs, _) if possiblyAnyRef =:= AnyRefType =>
        process(typeRefs)

      //Refined type such as "Foo with Bar with Baz"
      case RefinedType(typeRefs, _) =>
        process(typeRefs)

      //Existential type such as "Seq[_] forSome ..."
      case ExistentialType(/*quantified*/_, underlying) =>
        process(List(underlying))

      //Annotated type
      case AnnotatedType(_, underlying) =>
        process(List(underlying))

      //Standard type
      case TypeRef(_, _, args) =>
        process(args)

      //Unrecognized type
      case _ =>
        throw new IllegalStateException(s"Unable to determine type arguments for $t")
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

@scala.annotation.implicitNotFound("No TypeTagTree available for ${T}. If there are type arguments, please consider providing explicit types for all of them.")
sealed trait TypeTagTree[T] {
  /** The [[scala.reflect.runtime.universe.Type]] that this instance represents. */
  val tpe: scala.reflect.runtime.universe.Type

  /** The [[scala.reflect.runtime.universe.TypeSymbol]] that this instance represents. */
  val symbol: scala.reflect.runtime.universe.TypeSymbol

  /** The type arguments, if any, for this [[scala.reflect.runtime.universe.Type]]. */
  val typeArguments: Seq[TypeTagTree[_]]

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

  /** Determines if another instance is equal to this [[TypeTagTree]] instance. */
  override def equals(other: Any): Boolean =
    other match {
      case ref: TypeTagTree[_] if tpe == ref.tpe => true
      case ref: AnyRef => ref eq TypeTagTree.this
      case _ => false
    }
}

object TypeTagTree {
  import scala.reflect.runtime.universe._
  import scala.reflect.macros._

  import scala.language.experimental.macros

  /** Materializes an instance of a [[TypeTagTree]] for the provided type `T`. */
  def typeTagTreeOf[T](implicit ttt: TypeTagTree[T]): TypeTagTree[T] =
    ttt

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
   * the [[scala.reflect.runtime.universe.Type]], the type arguments, and the [[TypeTagTreeSource]]
   * for a [[TypeTagTree]].
   *
   * @param ttt The [[TypeTagTree]] instance that will be extracted
   * @tparam T The [[scala.reflect.runtime.universe.Type]] for the [[TypeTagTree]]
   * @return An extracted view of the provided [[TypeTagTree]] instance
   */
  def unapply[T](ttt: TypeTagTree[T]): Option[(TypeSymbol, Type, Seq[TypeTagTree[_]], TypeTagTreeSource)] =
    PartialFunction.condOpt(ttt) {
      case t => (t.symbol, t.tpe, t.typeArguments, t.source)
    }

  /**
   * Takes an existing higher-kinded [[TypeTagTree]] as a template and produces a new instance
   * with its type parameters replaced by the provided `typeParameters`.
   *
   * @param typeConstructorTemplate Higher-kinded [[TypeTagTree]] candidate that will have its
   *                                type parameters replaced
   * @param typeParameters Variable-length list of type parameters that will be used to replace
   *                       the template's type parameters
   * @tparam T Type of the template which will remain unchanged except for its type parameters
   * @return A new [[TypeTagTree]] with new type parameters
   */
  def alterTypeParameters[T](typeConstructorTemplate: TypeTagTree[T], typeParameters: TypeTagTree[_]*): TypeTagTree[T] =
    TypeTagTreeNode(
      typeConstructorTemplate.source,
      alterTypeParameters(typeConstructorTemplate.tpe, typeParameters.map(_.tpe):_*)
    )

  def alterTypeParameters(typeConstructorTemplate: Type, typeParameters: Type*): Type = {
    val asTypeRefApi = typeConstructorTemplate.asInstanceOf[TypeRefApi]
    val newType = scala.reflect.runtime.universe.internal.typeRef(asTypeRefApi.pre, asTypeRefApi.sym, typeParameters.toList)
    newType
  }

  private object Macros {
    import scala.reflect.macros._

    /**
     * Generates a [[TypeTagTree]] instance when applied to a given [[scala.reflect.runtime.universe.Type]].
     */
    def typeTagTree[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[TypeTagTree[T]] = {
      import c.universe._

      val typeParam = c.weakTypeOf[T]
      val pos = c.enclosingPosition

      /**
       * Generates an AST representing the following:
       *   `TypeTagTreeNode[T](TypeTagTreeSource(...))(&lt;implicitly discovered tag&gt;)`
       * `T` is the provided type and its implicit type tag is automagically
       * found and created by the compiler.
       */
      c.Expr[TypeTagTreeNode[T]] {
        q"_root_.com.github.davidhoyt.fluxmuster.TypeTagTreeNode[$typeParam](_root_.com.github.davidhoyt.fluxmuster.TypeTagTreeSource(${pos.source.toString()}, ${pos.line}, ${pos.column}, ${pos.start}))"
      }
    }
  }
}
