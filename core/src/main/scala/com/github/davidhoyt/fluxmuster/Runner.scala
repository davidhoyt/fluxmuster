package com.github.davidhoyt.fluxmuster

import scala.language.higherKinds

case class Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From[_], Into[_]](
  name: String,
  chain: ChainLink,
  link: Link[DownstreamIn, Into[UpstreamOut]],
  originalProxy: Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut],
  providedRunnerChain: ChainRunner,
  state: State,
  ops: RunnerOps[State, Into],
  converter: From -> Into,
  rewireOnFlatMap: Boolean = false,
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

  implicit lazy val toProxy: Proxy[DownstreamIn, DownstreamIn, Into[UpstreamOut], Into[UpstreamOut]] =
    Proxy(name, Link.identity[DownstreamIn](link.typeIn), Link.identity[Into[UpstreamOut]](link.typeOut), run)(link.typeIn, link.typeOut)

  lazy val runnerChain: ChainRunner = {
    val data = RunnerData(name, runner, state, ops)(converter, typeIn, typeOut, typeFrom, link.typeOut, typeState).asChainableRunner
    if ((providedRunnerChain eq null) || providedRunnerChain.isEmpty)
      newChainRunner(data)
    else
      providedRunnerChain :+ data
  }

  def replace[A, B, C, D](proxy: Proxy[A, B, C, D])(implicit typeFromOfD: TypeTagTree[From[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, From, Into] =
    Runner.withUnliftedProxy[A, B, C, D, State, From, Into](name, proxy, providedRunnerChain, state, ops, rewireOnFlatMap)(converter, typeState, typeFromOfD, typeIntoOfD)

  def combineHead[A, B, C, D](proxy: Proxy[A, B, C, D])(implicit a: B => DownstreamIn, b: B => DownstreamOut, c: UpstreamIn => C, d: UpstreamOut => C, typeFromOfD: TypeTagTree[From[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, DownstreamOut, UpstreamIn, D, State, From, Into] = {
    import scala.language.existentials

    implicit val convert = converter

    def replaceWith = {
      val mappedProxy =
        proxy.map { p =>
          Proxy("<~>", p.downstream.map(b)(originalProxy.downstream.typeOut), c.toLink(originalProxy.upstream.typeIn, proxy.upstream.typeIn) andThen p.upstream, originalProxy.proofDownstreamCanMapToUpstream.toFunction)(originalProxy.downstream.typeOut, originalProxy.upstream.typeIn)
        }

      Runner.withUnliftedProxy(name, mappedProxy, providedRunnerChain, state, ops, rewireOnFlatMap = false)(converter, typeState, typeFromOfD, typeIntoOfD)
    }

    def combineWithHead = {
      implicit val proof =
        originalProxy.proofDownstreamCanMapToUpstream.toFunction

      val newMappedProxy: Proxy[A, DownstreamOut, UpstreamIn, D] =
        proxy combine originalProxy

      val newMappedLink: Link[A, D] =
        newMappedProxy.toLink

      //Re-lift

      //There will always be at least one item in the runnerChain (this instance).
      val initial = {
        val rd = runnerChain.head
        val l = newMappedLink.asInstanceOf[ExistentialLink]
        val alteredTypeInto = TypeTagTree.alterTypeParameters(rd.typeInto, l.typeOut)
        val lifted = rd.ops.liftRunner(chain, l.toFunction)(rd.ops.unsafeCastAsState(rd.state), l.typeIn, l.typeOut).toLink(rd.name)(l.typeIn, alteredTypeInto)
        //val lifted2 = rd.ops.runInThisContext(chain, lifted.toFunction, rd.ops.unsafeCastAsState(rd.state))(rd.converter, lifted.typeIn, rd.typeFrom, rd.typeInto)
        val liftedChain = newChainLink(lifted)
        (liftedChain, lifted)
      }

      val relifted =
        runnerChain.tail.dropRight(1).foldLeft(initial) {
          case ((chainLink: ChainLink, link), rd) =>
            //It's necessary to change the type parameters because these TypeTagTrees were captured
            //with the proxy/link provided before it was possibly combined with other proxies.
            //As a result, the type parameters can (and are often wrong). The type constructor is
            //always correct, but we swap out for the correct type parameter which is always the
            //final proxy's result type since runners simply decorate each other (they're
            //recursive).
            val alteredTypeFrom = TypeTagTree.alterTypeParameters(rd.typeFrom, newMappedLink.typeOut)
            val alteredTypeInto = TypeTagTree.alterTypeParameters(rd.typeInto, newMappedLink.typeOut)

            val liftPreviousIntoCurrent = rd.ops.runInThisContext(chainLink, link.toFunction, rd.state)(rd.converter, link.typeIn, alteredTypeFrom, alteredTypeInto) //(rd.typeState, w.typeIn, w.typeOut) // rd.runInThisContext(chainLink, l).asInstanceOf[ExistentialLink]
            val chainNext = newChainLink(liftPreviousIntoCurrent)

            (chainNext, liftPreviousIntoCurrent)
        }

      //Re-cast the newly relifted link back into its expected category.
      val (newChain, newLink) = {
        val (reliftedChainLink, reliftedLink) = relifted
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

  def |>[S, G[_]](other: RunnerNeedsProxy[S, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) =
    lift(other)

  def lift[S, G[_]](other: RunnerNeedsProxy[S, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) = {
    val lift = other.ops.runInThisContext(chain, link.toFunction, other.state)(converter, link.typeIn, typeInto, typeGOfD)
    val chainNext = newChainLink(lift)
    Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, Into, G](other.name, chainNext, lift, originalProxy, runnerChain, other.state, other.ops, converter, rewireOnFlatMap = false)(other.typeState, typeInto)
  }

  def |>[A, B, C, D, S, F[_], G[_]](other: Runner[A, B, C, D, S, F, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) =
    lift(other)

  def lift[A, B, C, D, S, F[_], G[_]](other: Runner[A, B, C, D, S, F, G])(implicit converter: Into -> G, typeGOfD: TypeTagTree[G[UpstreamOut]]) = {
    val f = Runner.withRunner("|>", this, other.state, other.ops, rewireOnFlatMap = false)(converter, other.typeState, typeInto, typeGOfD)
    f
  }

  //TODO: Use RunnerData and create a runner chain w/ runners that don't take a proxy or a runner
  //      but can be lifted
  //      Serial("s1") |> Serial("s2") |> Async("a1") |> Async("a2") |> proxy
  //      proxy |> Serial("s1") |>
  //def lift[A, B, C, D, S, F[_], G[_]](other: Runner[A, B, C, D, S, F, G])()

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

  private[fluxmuster] def liftChained[A, D, S, F[_]](chained: Chained[A, D], state: S, ops: RunnerOps[S, F])(implicit typeOut: TypeTagTree[F[D]]): Link[A, F[D]] = {
    val chain = chained.chain
    val runner = chained.runner
    val liftedRunner = Link(ops.liftRunner[A, D](chain, runner)(state, chained.typeIn, chained.typeOut))(chained.typeIn, typeOut)
    liftedRunner
  }

  def withLink[A, B, C, D, S, F[_], G[_]](name: String, chain: ChainLink, link: Link[A, G[D]], originalProxy: Proxy[A, B, C, D], runnerChain: ChainRunner, state: S, ops: RunnerOps[S, G], rewireOnFlatMap: Boolean = false)(implicit converter: F -> G, typeState: TypeTagTree[S], typeFofD: TypeTagTree[F[D]]): Runner[A, B, C, D, S, F, G] =
    Runner[A, B, C, D, S, F, G](name, chain, link, originalProxy, runnerChain, state, ops, converter, rewireOnFlatMap)(typeState, typeFofD)

  def withUnliftedProxy[A, B, C, D, S, F[_], G[_]](name: String, proxy: Proxy[A, B, C, D], runnerChain: ChainRunner, state: S, ops: RunnerOps[S, G], rewireOnFlatMap: Boolean = false)(implicit converter: F -> G, typeState: TypeTagTree[S], typeFOfD: TypeTagTree[F[D]], typeGOfD: TypeTagTree[G[D]]): Runner[A, B, C, D, S, F, G] = {
    val proxyLink = proxy.toLink
    val liftedRunner = liftChained(proxyLink, state, ops)
    val liftedChain = immutable.Vector(proxy.downstream, proxy.proofDownstreamCanMapToUpstream, proxy.upstream)
    val lifted = withLink[A, B, C, D, S, F, G](name, liftedChain, liftedRunner, proxy, runnerChain, state, ops, rewireOnFlatMap)
    lifted
  }

  def withRunner[A, B, C, D, S, F[_], G[_], State, Into[_]](name: String, runner: Runner[A, B, C, D, S, F, G], state: State, ops: RunnerOps[State, Into], rewireOnFlatMap: Boolean = false)(implicit converter:  G -> Into, typeState: TypeTagTree[State], typeGOfD: TypeTagTree[G[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, G, Into] = {
    val link = ops.runInThisContext(runner.chain, runner.run _, state)(converter, runner.link.typeIn, runner.link.typeOut, typeIntoOfD)
    val chain = newChainLink(link)
    val r = Runner(name, chain, link, runner.originalProxy, runner.runnerChain, state, ops, converter, rewireOnFlatMap = true)
    r
  }
}

trait RunnerNeedsProxy[State, Into[_]] extends Named {
  val state: State
  val ops: RunnerOps[State, Into]
  implicit val converter: Into -> Into
  val typeState: TypeTagTree[State]
}

object RunnerNeedsProxy {
  private case class Build[State, Into[_]](name: String, state: State, ops: RunnerOps[State, Into])(implicit val converter: Into -> Into, val typeState: TypeTagTree[State]) extends RunnerNeedsProxy[State, Into]

  def apply[S, G[_]](name: String, state: S, ops: RunnerOps[S, G])(implicit converter: G -> G, typeState: TypeTagTree[S]): RunnerNeedsProxy[S, G] =
    Build(name, state, ops)
}

case class RunnerData[In, Out, State, From[_], Into[_]](name: String, runner: In => Out, state: State, ops: RunnerOps[State, Into])(implicit val converter: From -> Into, val typeIn: TypeTagTree[In], val typeOut: TypeTagTree[Out], val typeFrom: TypeTagTree[From[Out]], val typeInto: TypeTagTree[Into[Out]], val typeState: TypeTagTree[State]) {
  def asChainableRunner: ChainableRunner =
    this.asInstanceOf[ChainableRunner]
}
