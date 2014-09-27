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

  implicit val linkChain: LinkChain

  val sideEffects: ChainSideEffects[Out]

  val isIdentity = false

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
    //The addition to the linkChain based on types is not sound since you could have a function
    //that's provided that is not the identity function but yields a different value of the same
    //type. This would filter that out. In practice it would be very rare for this to occur and
    //if it becomes a problem, it will need to always be added to the chain. Unfortunately this
    //would proliferate a lot of identity functions for the very few cases where it could be useful.
    //In those cases it's suggested you simply compose a new link using the function in question.
    new Link.Build[In, OtherOut]("~>", Link.this.linkChain ++ (if (Link.this.typeOut != other.typeIn) Seq(Link(thisOutToOtherIn)(Link.this.typeOut, other.typeIn)) else Seq()), other.linkChain, Link.linkCombined)(Link.this.typeIn, other.typeOut) {
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
    //The addition to the linkChain based on types is not sound since you could have a function
    //that's provided that is not the identity function but yields a different value of the same
    //type. This would filter that out. In practice it would be very rare for this to occur and
    //if it becomes a problem, it will need to always be added to the chain. Unfortunately this
    //would proliferate a lot of identity functions for the very few cases where it could be useful.
    //In those cases it's suggested you simply compose a new link using the function in question.
    new Link.Build[OtherIn, Out]("<~", other.linkChain ++ (if (other.typeOut != Link.this.typeIn) Seq(Link(otherOutToThisIn)(other.typeOut, Link.this.typeIn)) else Seq()), Link.this.linkChain, Link.linkCombined)(other.typeIn, Link.this.typeOut) {
      protected def runLink[A, B](a: A)(implicit aToIn: A => OtherIn, outToB: Out => B): B =
        Link.this.apply(other.apply(aToIn(a))(identity, otherOutToThisIn))
    }

  override val hashCode: Int =
    (typeIn.hashCode() * 31) +
    typeOut.hashCode()

  override def equals(other: Any): Boolean =
    other match {
      case ref: Link[_, _] if isIdentity && ref.isIdentity && typeIn == ref.typeIn && typeOut == ref.typeOut =>
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

  private[fluxmuster] abstract case class Build[In, Out](name: String, mine: LinkChain, otherLink: LinkChain, chaining: FnChainLink, override val asShortString: String = null, sideEffects: ChainSideEffects[Out] = SideEffectsChainEmpty[Out], override val isIdentity: Boolean = false)(implicit val typeIn: TypeTagTree[In], val typeOut: TypeTagTree[Out]) extends Link[In, Out] with Named {
    lazy val linkChain =
      chainTogether(this, mine, otherLink)

    def chainTogether(instance: LinkAny, mine: LinkChain, other: LinkChain): LinkChain =
      chaining(instance, mine, other)
  }

  private[fluxmuster] def linkCombined(instance: LinkAny, mine: LinkChain, other: LinkChain): LinkChain =
    (mine ++ other).foldLeft(LinkChainEmpty) {
      case (seq, p) if p.linkChain.nonEmpty =>
        seq :+ p.linkChain.head
      case (seq, _) =>
        seq
    }
    //.filterNot(_.isIdentity)

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
    create(name, link.linkChain, LinkChainEmpty, linkProvided _, sideEffects.toVector)(link.run)(link.typeIn, link.typeOut)

  def create[In, Out](mine: LinkChain, other: LinkChain, chaining: FnChainLink)(fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](fn.toString(), mine, other, chaining, SideEffectsChainEmpty[Out])(fn)(typeIn, typeOut)

  def create[In, Out](name: String, mine: LinkChain, other: LinkChain, chaining: FnChainLink)(fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](name, mine, other, chaining, SideEffectsChainEmpty[Out])(fn)(typeIn, typeOut)

  def create[In, Out](name: String, mine: LinkChain, other: LinkChain, chaining: FnChainLink, sideEffects: ChainSideEffects[Out], identity: Boolean = false)(fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    new Build[In, Out](name, mine, other, chaining, s"${typeIn.toShortString} => ${typeOut.toShortString}", sideEffects, identity)(typeIn, typeOut) {
      protected def runLink[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        outToB(fn(aToIn(a)))
    }

  def identity[A](implicit typeA: TypeTagTree[A]): Link[A, A] = {
    val fn = Predef.identity[A] _
    create[A, A](fn.toString(), LinkChainEmpty, LinkChainEmpty, linkProvided _, SideEffectsChainEmpty[A], identity = true)(fn)(typeA, typeA)
  }
}
