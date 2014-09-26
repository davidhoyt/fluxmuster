package com.github.davidhoyt.fluxmuster

import Chains._

import scala.language.higherKinds

case class LiftChainEntry[In, Out, State, From[_], Into[_]](name: String, runner: In => Out, state: State, ops: LiftOps[State, Into])(implicit val converter: From -> Into, val typeFrom: TypeTagTree[From[Out]], val typeInto: TypeTagTree[Into[Out]]) {
  def asLiftChainEntryAny: LiftChainEntryAny =
    this.asInstanceOf[LiftChainEntryAny]
}

case class Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From[_], Into[_]](
  name: String,
  chain: LinkChain,
  link: Link[DownstreamIn, Into[UpstreamOut]],
  givenProxy: LinkedProxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut],
  givenLiftChain: LiftChain,
  state: State,
  ops: LiftOps[State, Into]
)(
  rewireOnFlatMap: Boolean = false,
  mapState: (State, LiftChain) => State = (s: State, _: LiftChain) => s,
  asShortString: String = null
)(
  implicit
  val converter: From -> Into,
  val typeState: TypeTagTree[State],
  val typeFrom: TypeTagTree[From[UpstreamOut]]
)
  extends Run[DownstreamIn, Into[UpstreamOut]]
  with Named {

  val typeIn =
    givenProxy.downstream.typeIn

  val typeOut =
    givenProxy.upstream.typeOut

  val typeInto =
    link.typeOut

  val runner =
    givenProxy.runner

  def apply[A, B](in: A)(implicit convert: A => DownstreamIn): Into[UpstreamOut] =
    run(convert(in))

  def run(in: DownstreamIn): Into[UpstreamOut] =
    link.run(in)

  def asLift: Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From, Into] =
    this.asInstanceOf[Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From, Into]]

  implicit lazy val toLink: Link[DownstreamIn, Into[UpstreamOut]] =
    link

  implicit lazy val toProxy: LinkedProxy[DownstreamIn, DownstreamIn, Into[UpstreamOut], Into[UpstreamOut]] =
    Proxy.linked(name, Link.identity[DownstreamIn](link.typeIn), Link.identity[Into[UpstreamOut]](link.typeOut), link)

  lazy val opsChain: LiftOpsChain =
    liftChain.map(_.ops.asLiftOpsAny)

  lazy val liftChain: LiftChain = {
    val data =
      LiftChainEntry(name, runner, state, ops)(converter, typeFrom, typeInto)
        .asLiftChainEntryAny

    if ((givenLiftChain eq null) || givenLiftChain.isEmpty)
      newLiftChain(data)
    else
      givenLiftChain :+ data
  }

  def replace[A, B, C, D](proxy: LinkedProxy[A, B, C, D])(implicit typeFromOfD: TypeTagTree[From[D]], typeIntoOfD: TypeTagTree[Into[D]]): Lift[A, B, C, D, State, From, Into] =
    Lift.proxy[A, B, C, D, State, From, Into](name, proxy, givenLiftChain, state, ops, rewireOnFlatMap)(converter, typeState, typeFromOfD, typeIntoOfD)

  def combineHead[A, B, C, D](proxy: LinkedProxy[A, B, C, D])(implicit a: B => DownstreamIn, b: B => DownstreamOut, c: UpstreamIn => C, d: UpstreamOut => C, typeFromOfD: TypeTagTree[From[D]], typeIntoOfD: TypeTagTree[Into[D]]): Lift[A, DownstreamOut, UpstreamIn, D, State, From, Into] = {
    import scala.language.existentials

    def replaceWith = {
      val mappedProxy =
        proxy.map { p =>
          Proxy.linked("<~>", p.downstream.map(b)(givenProxy.downstream.typeOut), c.toLink(givenProxy.upstream.typeIn, proxy.upstream.typeIn) andThen p.upstream, givenProxy.linkDownstreamToUpstream.toFunction)
        }

      Lift.proxy(name, mappedProxy, givenLiftChain, state, ops, rewireOnFlatMap = false)(converter, typeState, typeFromOfD, typeIntoOfD)
    }

    def combineWithHead = {
      implicit val proof =
        givenProxy.linkDownstreamToUpstream.toFunction

      val newMappedProxy: LinkedProxy[A, DownstreamOut, UpstreamIn, D] =
        proxy combine givenProxy

      val newMappedLink: Link[A, D] =
        newMappedProxy.toLink

      //Re-lift

      //There will always be at least one item in the liftChain (this instance).
      val initial = {
        val rd = liftChain.head
        val liftedOpsChain = newLiftOpsChain(rd.ops.asLiftOpsAny)
        val l = newMappedLink.asInstanceOf[LinkExistential]
        val alteredTypeInto = TypeTagTree.alterTypeParameters(rd.typeInto, l.typeOut)
        val lifted = rd.ops.liftRunner(chain, ChainedLiftOps(liftedOpsChain), l.toFunction)(rd.ops.unsafeCastAsState(rd.state), l.typeIn, l.typeOut).toLink(rd.name)(l.typeIn, alteredTypeInto)
        //val lifted2 = rd.ops.runInThisContext(chain, lifted.toFunction, rd.ops.unsafeCastAsState(rd.state))(rd.converter, lifted.typeIn, rd.typeFrom, rd.typeInto)
        val liftedChain = newLinkChain(lifted)
        (liftedChain, liftedOpsChain, lifted)
      }

      val relifted =
        liftChain.tail.dropRight(1).foldLeft(initial) {
          case ((chainLink: LinkChain, opsChain: LiftOpsChain, link), rd) =>
            //It's necessary to change the type parameters because these TypeTagTrees were captured
            //with the proxy/link provided before it was possibly combined with other proxies.
            //As a result, the type parameters can (and are often wrong). The type constructor is
            //always correct, but we swap out for the correct type parameter which is always the
            //final proxy's result type since lifts simply decorate each other (they're
            //recursive).
            val alteredTypeFrom = TypeTagTree.alterTypeParameters(rd.typeFrom, newMappedLink.typeOut)
            val alteredTypeInto = TypeTagTree.alterTypeParameters(rd.typeInto, newMappedLink.typeOut)

            val nextOpsChain = opsChain :+ rd.ops.asLiftOpsAny

            val liftPreviousIntoCurrent = rd.ops.runInThisContext(chainLink, ChainedLiftOps(nextOpsChain), link.toFunction, rd.state)(rd.converter, link.typeIn, alteredTypeFrom, alteredTypeInto) //(rd.typeState, w.typeIn, w.typeOut) // rd.runInThisContext(chainLink, l).asInstanceOf[ExistentialLink]
            val chainNext = newLinkChain(liftPreviousIntoCurrent)

            (chainNext, nextOpsChain, liftPreviousIntoCurrent)
        }

      //Re-cast the newly relifted link back into its expected category.
      val (newChain, newLink) = {
        val (reliftedChainLink, _, reliftedLink) = relifted
        (reliftedChainLink, reliftedLink.asInstanceOf[Link[A, Into[D]]])
      }

      Lift.link(name, newChain, newLink, newMappedProxy, liftChain, state, ops, rewireOnFlatMap = false)
    }

    if (rewireOnFlatMap)
      replaceWith
    else
      combineWithHead
  }

  def map[A, B, C, D, S, F[_], G[_], T](fn: this.type => T)(implicit evidence: T <:< Lift[A, B, C, D, S, F, G]): T =
    fn(this)

  def flatMap[A, B, C, D, S, F[_], G[_], T](fn: this.type => T)(implicit evidence: T <:< Run[A, G[D]]): T =
    fn(this)

  def |>[A >: DownstreamIn, D <: UpstreamOut, S, G[_]](other: PartialLift[A, D, S, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]): Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, Into, G] =
    lift(other)

  def lift[A >: DownstreamIn, D <: UpstreamOut, S, G[_]](other: PartialLift[A, D, S, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]): Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, Into, G] = {
    val lift = 
      other.ops.runInThisContext(chain, ChainedLiftOps[G](opsChain :+ other.ops.asLiftOpsAny), link.toFunction, other.state)(converter, link.typeIn, typeInto, typeGOfD)
    val links = 
      newLinkChain(lift)
    val lifted =
      Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, Into, G](other.name, links, lift, givenProxy, liftChain, other.state, other.ops)(rewireOnFlatMap = false)(converter, other.typeState, typeInto)
    lifted
  }

  def |>[A, B, C, D, S, F[_], G[_]](other: Lift[A, B, C, D, S, F, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) =
    lift(other)

  def lift[A, B, C, D, S, F[_], G[_]](other: Lift[A, B, C, D, S, F, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) =
    Lift.lift("|>", this, other.state, other.ops, rewireOnFlatMap = false)(converter, other.typeState, typeInto, typeGOfD)

  lazy val asDefaultString = {
    val in = typeIn.toShortString
    val out = typeInto.toShortString

    s"$name[$in, $out]"
  }

  lazy val toShortString = {
    val short = asShortString
    if (short ne null)
      short
    else
      asDefaultString
  }

  override def toString =
    toShortString
}

object Lift {
  import Chains._

  def link[A, B, C, D, S, F[_], G[_]](name: String, chain: LinkChain, link: Link[A, G[D]], proxy: LinkedProxy[A, B, C, D], liftChain: LiftChain, state: S, ops: LiftOps[S, G], rewireOnFlatMap: Boolean = false, mapState: (S, LiftChain) => S = (s: S, _: LiftChain) => s)(implicit converter: F -> G, typeState: TypeTagTree[S], typeFofD: TypeTagTree[F[D]]): Lift[A, B, C, D, S, F, G] =
    Lift[A, B, C, D, S, F, G](name, chain, link, proxy, liftChain, state, ops)(rewireOnFlatMap, mapState)

  def proxy[A, B, C, D, S, F[_], G[_]](name: String, proxy: LinkedProxy[A, B, C, D], liftChain: LiftChain, state: S, ops: LiftOps[S, G], rewireOnFlatMap: Boolean = false, mapState: (S, LiftChain) => S = (s: S, _: LiftChain) => s)(implicit converter: F -> G, typeState: TypeTagTree[S], typeFOfD: TypeTagTree[F[D]], typeGOfD: TypeTagTree[G[D]]): Lift[A, B, C, D, S, F, G] = {
    val proxyLink =
      proxy.toLink
    val liftedRunner =
      Link(ops.liftRunner[A, D](proxyLink.chain, ChainedLiftOps(ops.asLiftOpsAny), proxyLink.runner)(state, proxyLink.typeIn, proxyLink.typeOut))(proxyLink.typeIn, typeGOfD)
    val liftedLinkChain =
      newLinkChain(proxy.downstream, proxy.linkDownstreamToUpstream, proxy.upstream)
    val lifted =
      link[A, B, C, D, S, F, G](name, liftedLinkChain, liftedRunner, proxy, liftChain, state, ops, rewireOnFlatMap, mapState)
    lifted
  }

  def lift[A, B, C, D, S, F[_], G[_], State, Into[_]](name: String, lift: Lift[A, B, C, D, S, F, G], state: State, ops: LiftOps[State, Into], rewireOnFlatMap: Boolean = false, mapState: (State, LiftChain) => State = (s: State, _: LiftChain) => s)(implicit converter:  G -> Into, typeState: TypeTagTree[State], typeGOfD: TypeTagTree[G[D]], typeIntoOfD: TypeTagTree[Into[D]]): Lift[A, B, C, D, State, G, Into] = {
    val link =
      ops.runInThisContext(lift.chain, ChainedLiftOps[Into](lift.opsChain :+ ops.asLiftOpsAny), lift.run _, state)(converter, lift.link.typeIn, lift.link.typeOut, typeIntoOfD)
    val chain =
      newLinkChain(link)
    val lifted =
      Lift[A, B, C, D, State, G, Into](name, chain, link, lift.givenProxy, lift.liftChain, state, ops)(rewireOnFlatMap = true, mapState = mapState)
    lifted
  }
}

/**
 * Describes a [[Lift]] that still needs a [[LinkedProxy]] in order to work.
 */
trait PartialLift[-In, +Out, State, Into[_]] extends Named {
  import Chains._

  val state: State
  val ops: LiftOps[State, Into]
  implicit val converter: Into -> Into
  val typeState: TypeTagTree[State]

  override def toString =
    s"PartialLift($name)"
}

object PartialLift {
  private case class Build[In, Out, State, Into[_]](name: String, state: State, ops: LiftOps[State, Into])(implicit val converter: Into -> Into, val typeState: TypeTagTree[State]) extends PartialLift[In, Out, State, Into]

  def apply[In, Out, S, G[_]](name: String, state: S, ops: LiftOps[S, G])(implicit converter: G -> G, typeState: TypeTagTree[S]): PartialLift[In, Out, S, G] =
    Build[In, Out, S, G](name, state, ops)

  def withOut[Out, S, G[_]](name: String, state: S, ops: LiftOps[S, G])(implicit converter: G -> G, typeState: TypeTagTree[S]): PartialLift[_, Out, S, G] =
    Build(name, state, ops)
}
