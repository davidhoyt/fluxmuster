package com.github.davidhoyt.fluxmuster

/**
 * Represents a quasi-polymorphic function that can be composed with other
 * links and implicitly converted functions.
 *
 * Input and output types can be implicitly converted to and their type
 * information recorded for runtime reflection and all composed links are
 * tracked as a chain of links/functions that represent the combined
 * path to the current link.
 */
sealed trait Link[In, Out] extends Chained[In, Out] with Run[In, Out] { self: Named =>
  import scala.collection._

  implicit val typeIn: TypeTagTree[In]
  implicit val typeOut: TypeTagTree[Out]

  implicit val chain: ChainLink

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

  implicit lazy val toFunction: In => Out =
    run

  implicit lazy val toProxy: Proxy[In, In, Out, Out] =
    Proxy(name, Link.identity[In], Link.identity[Out], toFunction)(typeIn, typeOut)

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

  private[fluxmuster] abstract case class Build[In, Out](name: String, mine: ChainLink, otherLink: ChainLink, chaining: FnChainLink, override val asShortString: String = null, sideEffects: ChainSideEffects[Out] = EmptyChainSideEffects[Out])(implicit val typeIn: TypeTagTree[In], val typeOut: TypeTagTree[Out]) extends Link[In, Out] with Named {
    lazy val chain =
      chainTogether(this, mine, otherLink)

    def chainTogether(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
      chaining(instance, mine, other)
  }

  private[fluxmuster] def linkCombined(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
    (mine ++ other).foldLeft(EmptyChainLink) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }

  private[fluxmuster] def linkProvided(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
    if ((mine eq null) || mine.isEmpty)
      immutable.Vector(instance)
    else
      mine

  def apply[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](EmptyChainLink, EmptyChainLink, linkProvided _)(fn)(tIn, tOut)

  def apply[In, Out](name: String)(fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](name, EmptyChainLink, EmptyChainLink, linkProvided _, EmptyChainSideEffects[Out])(fn)(tIn, tOut)

  def withSideEffects[In, Out](name: String, link: Link[In, Out])(sideEffects: SideEffecting[Out]*): Link[In, Out] =
    create(name, link.chain, EmptyChainLink, linkProvided _, sideEffects.toVector)(link.run)(link.typeIn, link.typeOut)

  def create[In, Out](mine: ChainLink, other: ChainLink, chaining: FnChainLink)(fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](fn.toString(), mine, other, chaining, EmptyChainSideEffects[Out])(fn)(tIn, tOut)

  def create[In, Out](name: String, mine: ChainLink, other: ChainLink, chaining: FnChainLink)(fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    create[In, Out](name, mine, other, chaining, EmptyChainSideEffects[Out])(fn)(tIn, tOut)

  def create[In, Out](name: String, mine: ChainLink, other: ChainLink, chaining: FnChainLink, sideEffects: ChainSideEffects[Out])(fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    new Build[In, Out](name, mine, other, chaining, s"${tIn.toShortString} => ${tOut.toShortString}", sideEffects)(tIn, tOut) {
      protected def runLink[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
        outToB(fn(aToIn(a)))
    }

  def identity[A](implicit tA: TypeTagTree[A]): Link[A, A] =
    (Predef.identity[A]_).toLink(tA, tA)
}
