package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.TypeTagTree

/**
 * Represents a quasi-polymorphic function that can be composed with other
 * links and implicitly converted functions.
 *
 * Input and output types can be implicitly converted to and their type
 * information recorded for runtime reflection and all composed links are
 * tracked as a chain.
 */
trait Link { this: ChainingStrategy with Named =>
  import scala.collection._

  type In
  type Out

  implicit val typeIn: TypeTagTree[In]
  implicit val typeOut: TypeTagTree[Out]

  implicit val chain: ChainLink

  def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B

  def asShortString: String =
    null

  lazy val asDefaultString =
    s"$name[${typeIn.toShortString}, ${typeOut.toShortString}]"

  lazy val toShortString = {
    val short = asShortString
    if (short eq null)
      asDefaultString
    else
      short
  }

  override def toString =
    toShortString

  def asFunction[A, B](implicit aToIn: A => In, outToB: Out => B): A => B =
    (a: A) => apply(aToIn(a))(identity, outToB)

  def ~>[A, B](other: Link { type In = A; type Out = B })(implicit thisOutToOtherIn: Out => A): Linked[In, B] =
    andThen(other)(thisOutToOtherIn)

  def down[A, B](other: Link { type In = A; type Out = B })(implicit thisOutToOtherIn: Out => A): Linked[In, B] =
    andThen(other)(thisOutToOtherIn)

  def andThen[A, B](other: Link { type In = A; type Out = B })(implicit thisOutToOtherIn: Out => A): Linked[In, B] =
    new Link with CombinedChain with Named {
      val name = "~>"

      type In = Link.this.In
      type Out = other.Out

      val typeIn = Link.this.typeIn
      val typeOut = other.typeOut

      lazy val chain = chainTogether(Link.this.chain, other.chain)

      def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        other.apply(Link.this.apply(aToIn(a))(identity, thisOutToOtherIn))
    }

  def <~[A, B](other: Link { type In = A; type Out = B })(implicit otherOutToThisIn: B => In): Linked[A, Out] =
    compose(other)(otherOutToThisIn)

  def up[A, B](other: Link { type In = A; type Out = B })(implicit otherOutToThisIn: B => In): Linked[A, Out] =
    compose(other)(otherOutToThisIn)

  def compose[A, B](other: Link { type In = A; type Out = B })(implicit otherOutToThisIn: B => In): Linked[A, Out] =
    new Link with CombinedChain with Named {
      val name = "<~"

      type In = other.In
      type Out = Link.this.Out

      val typeIn = other.typeIn
      val typeOut = Link.this.typeOut

      lazy val chain = chainTogether(other.chain, Link.this.chain)

      def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        Link.this.apply(other.apply(aToIn(a))(identity, otherOutToThisIn))
    }

  override def hashCode: Int =
    typeIn.hashCode() * 31 + typeOut.hashCode()

  override def equals(other: Any): Boolean = other match {
    case ref: Link if typeIn == ref.typeIn && typeOut == ref.typeOut => true
    case ref: AnyRef => ref eq Link.this
    case _ => false
  }
}

object Link {
  private abstract case class Build[In0, Out0](name: String = "Link")(implicit val typeIn: TypeTagTree[In0], val typeOut: TypeTagTree[Out0]) extends ProvidedChain with Named with Link { type In = In0; type Out = Out0 }

  def apply[In0, Out0](fn: In0 => Out0)(implicit tIn: TypeTagTree[In0], tOut: TypeTagTree[Out0]): Linked[In0, Out0] =
    new Build[In0, Out0](fn.toString)(tIn, tOut) {
      override val asShortString =
        s"${tIn.toShortString} => ${tOut.toShortString}"

      lazy val chain = chainTogether(EmptyChainLink, EmptyChainLink)

      def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        outToB(fn(aToIn(a)))
    }
}
