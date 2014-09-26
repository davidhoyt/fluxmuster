package com.github.davidhoyt.fluxmuster

/**
 * Represents a quasi-polymorphic function that can be composed with other
 * links and implicitly converted functions.
 *
 * Input and output types can be implicitly converted to and their type
 * information recorded for runtime reflection and all composed links are
 * tracked as a chain of links/functions that represent the combined
 * path to the current link.
 *
 * Assuming all links in a composed chain are non-side-effecting, then
 * this allows for runtime introspection and possible execution plans
 * with plan optimizations and reordering as well as partitioning and
 * concurrent execution.
 */
sealed trait Link[In, Out]
  extends Chain[In, Out]
  with Run[In, Out] { self: Named =>

  import scala.collection._
  import Chains._

  implicit val typeIn: TypeTagTree[In]
  implicit val typeOut: TypeTagTree[Out]

  implicit val chain: LinkChain

  val sideEffects: ChainSideEffects[Out]

  protected def runLink[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B

  override def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B = {
    val out = runLink(a)(aToIn, identity)
    if (sideEffects.nonEmpty)
      sideEffects foreach (_.apply(out))
    outToB(out)
  }

  def run(in: In): Out =
    apply(in)(identity, identity)

  def foreach(fn: Out => Unit): Link[In, Out] =
    Link.withSideEffects(name, this)(sideEffects :+ fn :_*)

  def map[A](fn: Out => A)(implicit tA: TypeTagTree[A]): Link[In, A] =
    andThen(fn)(identity, typeOut, tA)

  def flatMap[A, B](fn: Out => Link[A, B])(implicit outToA: Out => A, tB: TypeTagTree[B]): Link[In, B] = {
    //Necessarily does not add to the chain and instead
    //creates a new chain where only this link is in it.
    Link((in: In) => {
      val out = Link.this.run(in)
      val newLink = fn(out)
      newLink.run(outToA(out))
    })(typeIn, tB)
  }

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

  override val toString =
    toShortString

  lazy val runner =
    toFunction

  implicit val toFunction: In => Out =
    run

  implicit lazy val toLinkedProxy: LinkedProxy[In, In, Out, Out] =
    Proxy.linked(name, Link.identity[In], Link.identity[Out], toFunction)

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
    new Link.Build[In, OtherOut]("~>", Link.this.chain ++ (if (Link.this.typeOut != other.typeIn) Seq(Link(thisOutToOtherIn)(Link.this.typeOut, other.typeIn)) else Seq()), other.chain, Link.linkCombined)(Link.this.typeIn, other.typeOut) {
      protected def runLink[A, B](a: A)(implicit aToIn: A => In, outToB: OtherOut => B): B =
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
    new Link.Build[OtherIn, Out]("<~", other.chain ++ (if (other.typeOut != Link.this.typeIn) Seq(Link(otherOutToThisIn)(other.typeOut, Link.this.typeIn)) else Seq()), Link.this.chain, Link.linkCombined)(other.typeIn, Link.this.typeOut) {
      protected def runLink[A, B](a: A)(implicit aToIn: A => OtherIn, outToB: Out => B): B =
        Link.this.apply(other.apply(aToIn(a))(identity, otherOutToThisIn))
    }

  override val hashCode: Int =
    (typeIn.hashCode() * 31) +
    typeOut.hashCode()

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
  import Chains._

  private[fluxmuster] abstract case class Build[In, Out](name: String, mine: LinkChain, otherLink: LinkChain, chaining: FnChainLink, override val asShortString: String = null, sideEffects: ChainSideEffects[Out] = SideEffectsChainEmpty[Out])(implicit val typeIn: TypeTagTree[In], val typeOut: TypeTagTree[Out]) extends Link[In, Out] with Named {
    lazy val chain =
      chainTogether(this, mine, otherLink)

    def chainTogether(instance: LinkAny, mine: LinkChain, other: LinkChain): LinkChain =
      chaining(instance, mine, other)
  }

  private[fluxmuster] def linkCombined(instance: LinkAny, mine: LinkChain, other: LinkChain): LinkChain =
    (mine ++ other).foldLeft(LinkChainEmpty) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }

  private[fluxmuster] def linkProvided(instance: LinkAny, mine: LinkChain, other: LinkChain): LinkChain =
    if ((mine eq null) || mine.isEmpty)
      immutable.Vector(instance)
    else
      mine

  def apply[In, Out](fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](LinkChainEmpty, LinkChainEmpty, linkProvided _)(fn)(typeIn, typeOut)

  def apply[In, Out](name: String)(fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](name, LinkChainEmpty, LinkChainEmpty, linkProvided _, SideEffectsChainEmpty[Out])(fn)(typeIn, typeOut)

  def withSideEffects[In, Out](name: String, link: Link[In, Out])(sideEffects: SideEffecting[Out]*): Link[In, Out] =
    create(name, link.chain, LinkChainEmpty, linkProvided _, sideEffects.toVector)(link.run)(link.typeIn, link.typeOut)

  def create[In, Out](mine: LinkChain, other: LinkChain, chaining: FnChainLink)(fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](fn.toString(), mine, other, chaining, SideEffectsChainEmpty[Out])(fn)(typeIn, typeOut)

  def create[In, Out](name: String, mine: LinkChain, other: LinkChain, chaining: FnChainLink)(fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](name, mine, other, chaining, SideEffectsChainEmpty[Out])(fn)(typeIn, typeOut)

  def create[In, Out](name: String, mine: LinkChain, other: LinkChain, chaining: FnChainLink, sideEffects: ChainSideEffects[Out])(fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    new Build[In, Out](name, mine, other, chaining, s"${typeIn.toShortString} => ${typeOut.toShortString}", sideEffects)(typeIn, typeOut) {
      protected def runLink[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        outToB(fn(aToIn(a)))
    }

  def identity[A](implicit typeA: TypeTagTree[A]): Link[A, A] =
    Link(Predef.identity[A]_)(typeA, typeA)
}
