package com.github.davidhoyt.fluxmuster5.runner

import com.github.davidhoyt.fluxmuster.TypeTagTree
import com.github.davidhoyt.fluxmuster5._

import scala.language.higherKinds

case class Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into[_]](
  name: String,
  chain: ChainLink,
  link: Link[DownstreamIn, Into[UpstreamOut]],
  originalProxy: Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut],
  opsChain: ChainOps,
  state: State,
  ops: RunnerOps[State, Into],
  rewireOnFlatMap: Boolean = false,
  asShortString: String = null
)(
  implicit val typeState: TypeTagTree[State]
  )
  extends Chained[DownstreamIn, UpstreamOut]
  with Named
{

  import com.github.davidhoyt.fluxmuster.TypeTagTree
  import com.github.davidhoyt.fluxmuster5._
  import scala.collection.immutable

  val typeIn =
    originalProxy.downstream.typeIn

  val typeOut =
    originalProxy.upstream.typeOut

  val runner =
    originalProxy.runner

  def apply[A, B](in: A)(implicit convert: A => DownstreamIn): Into[UpstreamOut] =
    run(convert(in))

  def run(in: DownstreamIn): Into[UpstreamOut] =
    link.run(in)

  implicit lazy val toLink: Link[DownstreamIn, Into[UpstreamOut]] =
    link

  implicit lazy val toProxy: Proxy[DownstreamIn, DownstreamIn, Into[UpstreamOut], Into[UpstreamOut]] =
    Proxy(name, Link.identity[DownstreamIn](link.typeIn), Link.identity[Into[UpstreamOut]](link.typeOut), run)(link.typeIn, link.typeOut)

  def runInThisContext[OtherIn, OtherOut, OtherFrom[_]](chained: Chained[OtherIn, OtherFrom[OtherOut]], otherRunner: OtherIn => OtherFrom[OtherOut], providedState: State)(implicit converter: OtherFrom -> Into, typeIn: TypeTagTree[OtherIn], typeFromOut: TypeTagTree[OtherFrom[OtherOut]], typeIntoOut: TypeTagTree[Into[OtherOut]]): Link[OtherIn, Into[OtherOut]] = {
    val chain = chained.chain
    Link((in: OtherIn) => {
      val runOtherInThisContext: OtherIn => Into[OtherFrom[OtherOut]] = ops.liftRunner(chain, otherRunner)(providedState, typeIn, typeFromOut)
      val resultAfterRunning: Into[OtherFrom[OtherOut]] = runOtherInThisContext(in)

      //flatMap!
      val mapResultBackIntoThisContext = ops.map(resultAfterRunning)(converter.apply)(providedState)
      val flattenedBackIntoThisContext: Into[OtherOut] = ops.flatten(mapResultBackIntoThisContext)(providedState)
      flattenedBackIntoThisContext
    })
  }

  def combineInReverse[A, B, C, D](proxy: Proxy[A, B, C, D])(implicit a: B => DownstreamIn, b: B => DownstreamOut, c: UpstreamIn => C, d: UpstreamOut => C, typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, DownstreamOut, UpstreamIn, D, State, Into] = {
    def replaceWith = {
      val mappedProxy =
        proxy.map {
          case (down, up) =>
            (down.map(b)(originalProxy.downstream.typeOut), c.toLink(originalProxy.upstream.typeIn, proxy.upstream.typeIn) andThen up )
        }
        .withProof(originalProxy.proofDownstreamCanMapToUpstream.toFunction)

      Runner.withUnliftedProxy(name, mappedProxy, /*EmptyChainRunner[Into],*/ opsChain, state, ops, rewireOnFlatMap = false)
    }

    def combineWithHead = {
      import originalProxy.proofDownstreamCanMapToUpstream.toFunction

      val newMappedProxy: Proxy[A, DownstreamOut, UpstreamIn, D] =
        proxy combine originalProxy

      val fnNewMappedLink: A => D =
        newMappedProxy.toFunction

      val newRunLink: Link[A, Into[D]] =
        Link(ops.liftRunner(newMappedProxy.chain, newMappedProxy.runner)(state, newMappedProxy.downstream.typeIn, newMappedProxy.upstream.typeOut))(proxy.downstream.typeIn, typeIntoOfD)

      //Shouldn't be lifted already!!
      //It should run in the lifted context of the runner
      //should be Long => Int, *not* Long => Try[Int]
      val liftedChainLink: ChainLink = newMappedProxy.chain
      //val liftedRunChain: ChainRunner[Into] = runChain //No change to list of Runner (lift) instances

      Runner.withLink(name, liftedChainLink, newRunLink, newMappedProxy, opsChain, state, ops, rewireOnFlatMap = false)
    }

    if (rewireOnFlatMap)
      replaceWith
    else
      combineWithHead
  }

  def map[A, B, C, D, S, F[_]](fn: Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into] => Runner[A, B, C, D, S, F]): Runner[A, B, C, D, S, F] = {
    val r = fn(this)
    println(s"Runner.map: $r")
    r
  }

  def flatMap[A, B, C, D, S, F[_]](fn: Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into] => Runner[A, B, C, D, S, F]): Runner[A, B, C, D, S, F] = {
    val r = fn(this)
    println(s"Runner.flatMap: $r")
    r
  }

  lazy val asDefaultString = {
    val in = typeIn.toShortString
    val out = typeOut.toShortString

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

  private[fluxmuster5] def liftChained[A, D, S, F[_]](chained: Chained[A, D], state: S, ops: RunnerOps[S, F])(implicit typeOut: TypeTagTree[F[D]]): Link[A, F[D]] = {
    val chain = chained.chain
    val runner = chained.runner
    val liftedRunner = Link(ops.liftRunner[A, D](chain, runner)(state, chained.typeIn, chained.typeOut))(chained.typeIn, typeOut)
    liftedRunner
  }

//  private[fluxmuster5] def runInThisContext[In, Out, From[_], State, Into[_]](chain: ChainLink, otherRunner: In => From[Out], providedState: State, ops: RunnerOps[State, Into])(implicit converter: From -> Into, typeIn: TypeTagTree[In], typeFromOut: TypeTagTree[From[Out]], typeIntoOut: TypeTagTree[Into[Out]]): Link[In, Into[Out]] = {
//    Link((in: In) => {
//      //Why is this never called???
//      println("######################## BOO")
//      val runOtherInThisContext: In => Into[From[Out]] = ops.liftRunner(chain, otherRunner)(providedState, typeIn, typeFromOut)
//      val resultAfterRunning: Into[From[Out]] = runOtherInThisContext(in)
//
//      //flatMap!
//      val mapResultBackIntoThisContext = ops.map(resultAfterRunning)(converter.apply)(providedState)
//      val flattenedBackIntoThisContext: Into[Out] = ops.flatten(mapResultBackIntoThisContext)(providedState)
//      flattenedBackIntoThisContext
//    })
//  }

  def withLink[A, B, C, D, S, F[_]](name: String, chain: ChainLink, link: Link[A, F[D]], originalProxy: Proxy[A, B, C, D], opsChain: ChainOps, state: S, ops: RunnerOps[S, F], rewireOnFlatMap: Boolean = false)(implicit typeState: TypeTagTree[S]): Runner[A, B, C, D, S, F] =
    Runner[A, B, C, D, S, F](name, chain, link, originalProxy, opsChain, state, ops, rewireOnFlatMap)(typeState)

  def withUnliftedProxy[A, B, C, D, S, F[_]](name: String, proxy: Proxy[A, B, C, D], opsChain: ChainOps, state: S, ops: RunnerOps[S, F], rewireOnFlatMap: Boolean = false)(implicit typeState: TypeTagTree[S], typeFOfD: TypeTagTree[F[D]]): Runner[A, B, C, D, S, F] = {
    val proxyLink = proxy.toLink
    val liftedRunner = liftChained(proxyLink, state, ops)
    val liftedChain = immutable.Vector(proxy.downstream, proxy.proofDownstreamCanMapToUpstream, proxy.upstream)
    val lifted = withLink[A, B, C, D, S, F](name, liftedChain, liftedRunner, proxy, opsChain, state, ops, rewireOnFlatMap)
    lifted
  }

  def withRunner[A, B, C, D, S, F[_], State, Into[_]](name: String, runner: Runner[A, B, C, D, S, F], state: State, ops: RunnerOps[State, Into], rewireOnFlatMap: Boolean = false)(implicit converter:  F -> Into, typeState: TypeTagTree[State], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, Into] = {
//    val proxy = runner.originalProxy
//    val link: Link[A, F[D]] = runner.toLink
//    val chain: ChainLink = link.chain
//    val run = link.runner
//    val bar: Link[A, Into[D]] = runInThisContext(chain, run, state, ops)(converter, link.typeIn, link.typeOut, typeIntoOfD)
//    val liftedRunner: Runner[A, B, C, D, State, Into] = Runner(name, bar.chain, bar, proxy, ops.asChainableOps +: runner.opsChain, state, ops, rewireOnFlatMap)
//    liftedRunner
    ???
  }
}