package com.github.davidhoyt.fluxmuster4

import com.github.davidhoyt.fluxmuster.TypeTagTree

/**
 * Represents a quasi-polymorphic function that can be composed with other
 * links and implicitly converted functions.
 *
 * Input and output types can be implicitly converted to and their type
 * information recorded for runtime reflection and all composed links are
 * tracked as a chain.
 */
trait Link[In, Out] extends Chained { self: Named =>
  import scala.collection._

  implicit val typeIn: TypeTagTree[In]
  implicit val typeOut: TypeTagTree[Out]

  implicit val chain: ChainLink

  def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B

  def run(in: In): Out =
    apply(in)(identity, identity)

  def runAny(in: Any): Any =
    apply(in.asInstanceOf[In])(identity, identity)

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

  implicit lazy val toFunction: In => Out =
    run

  def asFunction[A, B](implicit aToIn: A => In, outToB: Out => B): A => B =
    (a: A) => apply(aToIn(a))(identity, outToB)

  def ~>[OtherIn, OtherOut](other: OtherIn => OtherOut)(implicit thisOutToOtherIn: Out => OtherIn, tOtherIn: TypeTagTree[OtherIn], tOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
    andThen(Link(other)(tOtherIn, tOtherOut))(thisOutToOtherIn)

  def ~>[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Link[In, OtherOut] =
    andThen(other)(thisOutToOtherIn)

  def down[OtherIn, OtherOut](other: OtherIn => OtherOut)(implicit thisOutToOtherIn: Out => OtherIn, tOtherIn: TypeTagTree[OtherIn], tOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
    andThen(Link(other)(tOtherIn, tOtherOut))(thisOutToOtherIn)

  def down[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Link[In, OtherOut] =
    andThen(other)(thisOutToOtherIn)

  def andThen[OtherIn, OtherOut](other: OtherIn => OtherOut)(implicit thisOutToOtherIn: Out => OtherIn, tOtherIn: TypeTagTree[OtherIn], tOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
    andThen(Link(other)(tOtherIn, tOtherOut))(thisOutToOtherIn)

  def andThen[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Link[In, OtherOut] =
    new Link.Build[In, OtherOut]("~>", Link.this.chain :+ Link(thisOutToOtherIn)(Link.this.typeOut, other.typeIn), other.chain, Link.linkCombined)(Link.this.typeIn, other.typeOut) {
      def apply[A, B](a: A)(implicit aToIn: A => In, outToB: OtherOut => B): B =
        other.apply(Link.this.apply(aToIn(a))(identity, thisOutToOtherIn))
    }

  def <~[OtherIn, OtherOut](other: OtherIn => OtherOut)(implicit otherOutToThisIn: OtherOut => In, tOtherIn: TypeTagTree[OtherIn], tOtherOut: TypeTagTree[OtherOut]): Link[OtherIn, Out] =
    compose(Link(other)(tOtherIn, tOtherOut))(otherOutToThisIn)

  def <~[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Link[OtherIn, Out] =
    compose(other)(otherOutToThisIn)

  def up[OtherIn, OtherOut](other: OtherIn => OtherOut)(implicit otherOutToThisIn: OtherOut => In, tOtherIn: TypeTagTree[OtherIn], tOtherOut: TypeTagTree[OtherOut]): Link[OtherIn, Out] =
    compose(Link(other)(tOtherIn, tOtherOut))(otherOutToThisIn)

  def up[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Link[OtherIn, Out] =
    compose(other)(otherOutToThisIn)

  def compose[OtherIn, OtherOut](other: OtherIn => OtherOut)(implicit otherOutToThisIn: OtherOut => In, tOtherIn: TypeTagTree[OtherIn], tOtherOut: TypeTagTree[OtherOut]): Link[OtherIn, Out] =
    compose(Link(other)(tOtherIn, tOtherOut))(otherOutToThisIn)

  def compose[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Link[OtherIn, Out] =
    new Link.Build[OtherIn, Out]("<~", other.chain :+ Link(otherOutToThisIn)(other.typeOut, Link.this.typeIn), Link.this.chain, Link.linkCombined)(other.typeIn, Link.this.typeOut) {
      def apply[A, B](a: A)(implicit aToIn: A => OtherIn, outToB: Out => B): B =
        Link.this.apply(other.apply(aToIn(a))(identity, otherOutToThisIn))
    }

  override def hashCode: Int =
    typeIn.hashCode() * 31 + typeOut.hashCode()

  override def equals(other: Any): Boolean =
    other match {
      case ref: Link[_, _] if typeIn == ref.typeIn && typeOut == ref.typeOut =>
        true
      case ref: AnyRef =>
        ref eq Link.this
      case _ =>
        false
    }
}

object Link {
  import scala.collection.immutable

  private[fluxmuster4] abstract case class Build[In, Out](name: String, mine: ChainLink, otherLink: ChainLink, chaining: FnChainLink, override val asShortString: String = null)(implicit val typeIn: TypeTagTree[In], val typeOut: TypeTagTree[Out]) extends Link[In, Out] with Named {
    lazy val chain =
      chainTogether(this, mine, otherLink)

    def chainTogether(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
      chaining(instance, mine, other)
  }

  private[fluxmuster4] def linkCombined(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
    (mine ++ other).foldLeft(EmptyChainLink) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }

  private[fluxmuster4] def linkProvided(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
    if ((mine eq null) || mine.isEmpty)
      immutable.Seq(instance)
    else
      mine

  def apply[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    create(EmptyChainLink, EmptyChainLink, linkProvided)(fn)(tIn, tOut)

  def apply[In, Out](name: String)(fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    create(name, EmptyChainLink, EmptyChainLink, linkProvided)(fn)(tIn, tOut)

  def create[In, Out](mine: ChainLink, other: ChainLink, chaining: FnChainLink)(fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    create(fn.toString(), mine, other, chaining)(fn)

  def create[In, Out](name: String, mine: ChainLink, other: ChainLink, chaining: FnChainLink)(fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    new Build[In, Out](name, mine, other, chaining, s"${tIn.toShortString} => ${tOut.toShortString}")(tIn, tOut) {
      def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        outToB(fn(aToIn(a)))
    }

  def identity[A](implicit tA: TypeTagTree[A]): Link[A, A] =
    (Predef.identity[A]_).toLink(tA, tA)
}
