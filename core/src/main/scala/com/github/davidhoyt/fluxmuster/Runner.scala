package com.github.davidhoyt.fluxmuster

import Chains._

import scala.language.higherKinds

case class Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From[_], Into[_]](
  name: String,
  chain: LinkChain,
  link: Link[DownstreamIn, Into[UpstreamOut]],
  originalProxy: LinkedProxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut],
  providedRunnerChain: RunnerDataChain,
  providedState: State,
  ops: RunnerOps[State, Into],
  converter: From -> Into,
  rewireOnFlatMap: Boolean = false,
  mapState: (State, RunnerDataChain) => State = (s: State, _: RunnerDataChain) => s,
  asShortString: String = null
)(
  implicit val typeState: TypeTagTree[State],
  val typeFrom: TypeTagTree[From[UpstreamOut]]
)
  extends Run[DownstreamIn, Into[UpstreamOut]]
  with Named
{

  val typeIn =
    originalProxy.downstream.typeIn

  val typeOut =
    originalProxy.upstream.typeOut

  val typeInto =
    link.typeOut

  //Create alternative runner chain so there's not a mutually recursive lazy evaluation
  //problem where loading the state loads the runner chain which loads the state, etc.
  lazy val state =
    providedState
    //mapState(providedState, createRunnerChain(providedState))

  val runner =
    originalProxy.runner

  def apply[A, B](in: A)(implicit convert: A => DownstreamIn): Into[UpstreamOut] =
    run(convert(in))

  def run(in: DownstreamIn): Into[UpstreamOut] =
    link.run(in)

  def asRunner: Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From, Into] =
    this.asInstanceOf[Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From, Into]]

  implicit lazy val toLink: Link[DownstreamIn, Into[UpstreamOut]] =
    link

  implicit lazy val toProxy: LinkedProxy[DownstreamIn, DownstreamIn, Into[UpstreamOut], Into[UpstreamOut]] =
    Proxy.linked(name, Link.identity[DownstreamIn](link.typeIn), Link.identity[Into[UpstreamOut]](link.typeOut), link)

  lazy val chainRunnerOps: RunnerOpsChain =
    runnerChain.map(_.ops.asChainableRunnerOps)

  lazy val runnerChain: RunnerDataChain =
    createRunnerChain(state)

  private def createRunnerChain(given: State): RunnerDataChain = {
    val data =
      RunnerData(name, runner, given, ops)(converter, typeIn, typeOut, typeFrom, link.typeOut, typeState)
        .asChainableRunner

    if ((providedRunnerChain eq null) || providedRunnerChain.isEmpty)
      newRunnerDataChain(data)
    else
      providedRunnerChain :+ data
  }

  def replace[A, B, C, D](proxy: LinkedProxy[A, B, C, D])(implicit typeFromOfD: TypeTagTree[From[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, From, Into] =
    Runner.withUnliftedProxy[A, B, C, D, State, From, Into](name, proxy, providedRunnerChain, state, ops, rewireOnFlatMap)(converter, typeState, typeFromOfD, typeIntoOfD)

  def combineHead[A, B, C, D](proxy: LinkedProxy[A, B, C, D])(implicit a: B => DownstreamIn, b: B => DownstreamOut, c: UpstreamIn => C, d: UpstreamOut => C, typeFromOfD: TypeTagTree[From[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, DownstreamOut, UpstreamIn, D, State, From, Into] = {
    import scala.language.existentials

    implicit val convert = converter

    def replaceWith = {
      val mappedProxy =
        proxy.map { p =>
          Proxy.linked("<~>", p.downstream.map(b)(originalProxy.downstream.typeOut), c.toLink(originalProxy.upstream.typeIn, proxy.upstream.typeIn) andThen p.upstream, originalProxy.linkDownstreamToUpstream.toFunction)
        }

      Runner.withUnliftedProxy(name, mappedProxy, providedRunnerChain, state, ops, rewireOnFlatMap = false)(converter, typeState, typeFromOfD, typeIntoOfD)
    }

    def combineWithHead = {
      implicit val proof =
        originalProxy.linkDownstreamToUpstream.toFunction

      val newMappedProxy: LinkedProxy[A, DownstreamOut, UpstreamIn, D] =
        proxy combine originalProxy

      val newMappedLink: Link[A, D] =
        newMappedProxy.toLink

      //Re-lift

      //There will always be at least one item in the runnerChain (this instance).
      val initial = {
        val rd = runnerChain.head
        val liftedOpsChain = newRunnerOpsChain(rd.ops.asChainableRunnerOps)
        val l = newMappedLink.asInstanceOf[LinkExistential]
        val alteredTypeInto = TypeTagTree.alterTypeParameters(rd.typeInto, l.typeOut)
        val lifted = rd.ops.liftRunner(chain, ChainedRunnerOps(liftedOpsChain), l.toFunction)(rd.ops.unsafeCastAsState(rd.state), l.typeIn, l.typeOut).toLink(rd.name)(l.typeIn, alteredTypeInto)
        //val lifted2 = rd.ops.runInThisContext(chain, lifted.toFunction, rd.ops.unsafeCastAsState(rd.state))(rd.converter, lifted.typeIn, rd.typeFrom, rd.typeInto)
        val liftedChain = newLinkChain(lifted)
        (liftedChain, liftedOpsChain, lifted)
      }

      val relifted =
        runnerChain.tail.dropRight(1).foldLeft(initial) {
          case ((chainLink: LinkChain, opsChain: RunnerOpsChain, link), rd) =>
            //It's necessary to change the type parameters because these TypeTagTrees were captured
            //with the proxy/link provided before it was possibly combined with other proxies.
            //As a result, the type parameters can (and are often wrong). The type constructor is
            //always correct, but we swap out for the correct type parameter which is always the
            //final proxy's result type since runners simply decorate each other (they're
            //recursive).
            val alteredTypeFrom = TypeTagTree.alterTypeParameters(rd.typeFrom, newMappedLink.typeOut)
            val alteredTypeInto = TypeTagTree.alterTypeParameters(rd.typeInto, newMappedLink.typeOut)

            val nextOpsChain = opsChain :+ rd.ops.asChainableRunnerOps

            val liftPreviousIntoCurrent = rd.ops.runInThisContext(chainLink, ChainedRunnerOps(nextOpsChain), link.toFunction, rd.state)(rd.converter, link.typeIn, alteredTypeFrom, alteredTypeInto) //(rd.typeState, w.typeIn, w.typeOut) // rd.runInThisContext(chainLink, l).asInstanceOf[ExistentialLink]
            val chainNext = newLinkChain(liftPreviousIntoCurrent)

            (chainNext, nextOpsChain, liftPreviousIntoCurrent)
        }

      //Re-cast the newly relifted link back into its expected category.
      val (newChain, newLink) = {
        val (reliftedChainLink, _, reliftedLink) = relifted
        (reliftedChainLink, reliftedLink.asInstanceOf[Link[A, Into[D]]])
      }

      Runner.withLink(name, newChain, newLink, newMappedProxy, runnerChain, state, ops, rewireOnFlatMap = false) //(convertInto, typeState, typeIntoOfD)
    }

    if (rewireOnFlatMap)
      replaceWith
    else
      combineWithHead
  }

  def map[A, B, C, D, S, F[_], G[_], T](fn: this.type => T)(implicit evidence: T <:< Runner[A, B, C, D, S, F, G]): T =
    fn(this)

  def flatMap[A, B, C, D, S, F[_], G[_], T](fn: this.type => T)(implicit evidence: T <:< Run[A, G[D]]): T =
    fn(this)

  def |>[A >: DownstreamIn, D <: UpstreamOut, S, G[_]](other: RunnerNeedsProxy[A, D, S, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]): Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, Into, G] =
    lift(other)

  def lift[A >: DownstreamIn, D <: UpstreamOut, S, G[_]](other: RunnerNeedsProxy[A, D, S, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]): Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, Into, G] = {
    val newChainedRunnerOps = chainRunnerOps :+ other.ops.asChainableRunnerOps
    val lift = other.ops.runInThisContext(chain, ChainedRunnerOps[G](newChainedRunnerOps), link.toFunction, other.state)(converter, link.typeIn, typeInto, typeGOfD)
    val links = newLinkChain(lift)
    val runner = Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, Into, G](other.name, links, lift, originalProxy, runnerChain, other.state, other.ops, converter, mapState = other.mapState, rewireOnFlatMap = false)(other.typeState, typeInto)
    runner
  }

  def |>[A, B, C, D, S, F[_], G[_]](other: Runner[A, B, C, D, S, F, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) =
    lift(other)

  def lift[A, B, C, D, S, F[_], G[_]](other: Runner[A, B, C, D, S, F, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) =
    Runner.withRunner("|>", this, other.state, other.ops, rewireOnFlatMap = false)(converter, other.typeState, typeInto, typeGOfD)

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

object Runner {
  import scala.collection.immutable
  import Chains._

  private[fluxmuster] def liftChained[A, D, S, F[_]](chained: Chain[A, D], state: S, ops: RunnerOps[S, F])(implicit typeOut: TypeTagTree[F[D]]): Link[A, F[D]] = {
    val chain = chained.chain
    val runner = chained.runner
    val liftedRunner = Link(ops.liftRunner[A, D](chain, ChainedRunnerOps(ops.asChainableRunnerOps), runner)(state, chained.typeIn, chained.typeOut))(chained.typeIn, typeOut)
    liftedRunner
  }

  def withLink[A, B, C, D, S, F[_], G[_]](name: String, chain: LinkChain, link: Link[A, G[D]], originalProxy: LinkedProxy[A, B, C, D], runnerChain: RunnerDataChain, state: S, ops: RunnerOps[S, G], rewireOnFlatMap: Boolean = false, mapState: (S, RunnerDataChain) => S = (s: S, _: RunnerDataChain) => s)(implicit converter: F -> G, typeState: TypeTagTree[S], typeFofD: TypeTagTree[F[D]]): Runner[A, B, C, D, S, F, G] =
    Runner[A, B, C, D, S, F, G](name, chain, link, originalProxy, runnerChain, state, ops, converter, rewireOnFlatMap, mapState)(typeState, typeFofD)

  def withUnliftedProxy[A, B, C, D, S, F[_], G[_]](name: String, proxy: LinkedProxy[A, B, C, D], runnerChain: RunnerDataChain, state: S, ops: RunnerOps[S, G], rewireOnFlatMap: Boolean = false, mapState: (S, RunnerDataChain) => S = (s: S, _: RunnerDataChain) => s)(implicit converter: F -> G, typeState: TypeTagTree[S], typeFOfD: TypeTagTree[F[D]], typeGOfD: TypeTagTree[G[D]]): Runner[A, B, C, D, S, F, G] = {
    val proxyLink = proxy.toLink
    val liftedRunner = liftChained(proxyLink, state, ops)
    val liftedChain = immutable.Vector(proxy.downstream, proxy.linkDownstreamToUpstream, proxy.upstream)
    val lifted = withLink[A, B, C, D, S, F, G](name, liftedChain, liftedRunner, proxy, runnerChain, state, ops, rewireOnFlatMap, mapState)
    lifted
  }

  def withRunner[A, B, C, D, S, F[_], G[_], State, Into[_]](name: String, runner: Runner[A, B, C, D, S, F, G], state: State, ops: RunnerOps[State, Into], rewireOnFlatMap: Boolean = false, mapState: (State, RunnerDataChain) => State = (s: State, _: RunnerDataChain) => s)(implicit converter:  G -> Into, typeState: TypeTagTree[State], typeGOfD: TypeTagTree[G[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, G, Into] = {
    val newChainRunnerOps = runner.chainRunnerOps :+ ops.asChainableRunnerOps
    val link = ops.runInThisContext(runner.chain, ChainedRunnerOps[Into](newChainRunnerOps), runner.run _, state)(converter, runner.link.typeIn, runner.link.typeOut, typeIntoOfD)
    val chain = newLinkChain(link)
    val r = Runner[A, B, C, D, State, G, Into](name, chain, link, runner.originalProxy, runner.runnerChain, state, ops, converter, rewireOnFlatMap = true, mapState = mapState)
    r
  }
}

trait RunnerNeedsProxy[-In, +Out, State, Into[_]] extends Named {
  import Chains._

  val state: State
  val ops: RunnerOps[State, Into]
  implicit val converter: Into -> Into
  val mapState: (State, RunnerDataChain) => State //TODO: REMOVE
  val typeState: TypeTagTree[State]
}

object RunnerNeedsProxy {
  private case class Build[In, Out, State, Into[_]](name: String, state: State, ops: RunnerOps[State, Into], mapState: (State, RunnerDataChain) => State)(implicit val converter: Into -> Into, val typeState: TypeTagTree[State]) extends RunnerNeedsProxy[In, Out, State, Into]

  def apply[In, Out, S, G[_]](name: String, state: S, ops: RunnerOps[S, G], mapState: (S, RunnerDataChain) => S = (s: S, _: RunnerDataChain) => s)(implicit converter: G -> G, typeState: TypeTagTree[S]): RunnerNeedsProxy[In, Out, S, G] =
    Build[In, Out, S, G](name, state, ops, mapState)
  def withOut[Out, S, G[_]](name: String, state: S, ops: RunnerOps[S, G], mapState: (S, RunnerDataChain) => S = (s: S, _: RunnerDataChain) => s)(implicit converter: G -> G, typeState: TypeTagTree[S]): RunnerNeedsProxy[_, Out, S, G] =
    Build(name, state, ops, mapState)
}

case class RunnerData[In, Out, State, From[_], Into[_]](name: String, runner: In => Out, state: State, ops: RunnerOps[State, Into])(implicit val converter: From -> Into, val typeIn: TypeTagTree[In], val typeOut: TypeTagTree[Out], val typeFrom: TypeTagTree[From[Out]], val typeInto: TypeTagTree[Into[Out]], val typeState: TypeTagTree[State]) {
  def asChainableRunner: RunnerDataAny =
    this.asInstanceOf[RunnerDataAny]
}
