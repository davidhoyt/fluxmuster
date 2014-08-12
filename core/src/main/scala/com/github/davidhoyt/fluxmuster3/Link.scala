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
trait Link[In, Out] { this: LinkChainingStrategy with Named =>
  import scala.collection._

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

  def ~>[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Linked[In, OtherOut] =
    andThen(other)(thisOutToOtherIn)

  def down[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Linked[In, OtherOut] =
    andThen(other)(thisOutToOtherIn)

  def andThen[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Linked[In, OtherOut] =
    new Link[In, OtherOut] with LinkCombinedChain with Named {
      val name = "~>"

      val typeIn = Link.this.typeIn
      val typeOut = other.typeOut

      lazy val chain = chainTogether(Link.this.chain, other.chain)

      def apply[A, B](a: A)(implicit aToIn: A => In, outToB: OtherOut => B): B =
        other.apply(Link.this.apply(aToIn(a))(identity, thisOutToOtherIn))
    }

  def <~[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Linked[OtherIn, Out] =
    compose(other)(otherOutToThisIn)

  def up[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Linked[OtherIn, Out] =
    compose(other)(otherOutToThisIn)

  def compose[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Linked[OtherIn, Out] =
    new Link[OtherIn, Out] with LinkCombinedChain with Named {
      val name = "<~"

      val typeIn = other.typeIn
      val typeOut = Link.this.typeOut

      lazy val chain = chainTogether(other.chain, Link.this.chain)

      def apply[A, B](a: A)(implicit aToIn: A => OtherIn, outToB: Out => B): B =
        Link.this.apply(other.apply(aToIn(a))(identity, otherOutToThisIn))
    }

  override def hashCode: Int =
    typeIn.hashCode() * 31 + typeOut.hashCode()

  override def equals(other: Any): Boolean =
    other match {
      case ref: Link[_, _] if typeIn == ref.typeIn && typeOut == ref.typeOut => true
      case ref: AnyRef => ref eq Link.this
      case _ => false
    }
}

object Link {
  private abstract case class Build[In0, Out0](name: String = "Link", override val asShortString: String = null)(implicit val typeIn: TypeTagTree[In0], val typeOut: TypeTagTree[Out0]) extends LinkProvidedChain with Named with Link[In0, Out0] {
  }

  def apply[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Linked[In, Out] =
    new Build[In, Out](fn.toString, s"${tIn.toShortString} => ${tOut.toShortString}")(tIn, tOut) {
      lazy val chain = chainTogether(EmptyChainLink, EmptyChainLink)

      def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        outToB(fn(aToIn(a)))
    }

  def identity[A](implicit tA: TypeTagTree[A]): Linked[A, A] =
    (Predef.identity[A]_).toLink(tA, tA)
}
