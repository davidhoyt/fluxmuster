package com.github.davidhoyt.fluxmuster5.runner

import com.github.davidhoyt.fluxmuster.TypeTagTree
import com.github.davidhoyt.fluxmuster5._

import scala.language.higherKinds

trait Runner[In, Out, State, Into[_]]
  extends Chained[In, Into[Out]]
{
  self: Named =>

  import com.github.davidhoyt.fluxmuster.TypeTagTree
  import com.github.davidhoyt.fluxmuster5._
  import scala.collection.immutable

  implicit val ops: RunnerOps[State, Into]
  implicit val state: State
  implicit val typeState: TypeTagTree[State]
  val runChain: ChainRunner[Into]

  val rewireOnFlatMap = false
  val originalLink: Link[In, Out]

  def apply[A, B](in: A)(implicit convert: A => In): Into[Out] =
    run(convert(in))

  def run(in: In): Into[Out] =
    runner(in)

  implicit lazy val toLink: Link[In, Into[Out]] =
    Link(name)(run)(typeIn, typeOut)

  implicit lazy val toProxy: Proxy[In, In, In, Into[Out]] =
    Proxy(name, Link.identity[In](typeIn), toLink, identity[In])(typeIn, typeIn)

  protected def mapStateOnRun(state: State, other: ChainableRunner[Into]): State =
    state

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

  private[fluxmuster5] def replaceLink[A, D](link: Link[A, D])(implicit proof: D => Out): Runner[A, Out, State, Into] = {
    Runner.withUnliftedLink(name, link.map(proof)(originalLink.typeOut), EmptyChainRunner[Into], state, ops, rewireOnFlatMap = false, mapStateOnRun _)(typeState, link.typeIn, typeOut)
  }

  private[fluxmuster5] def linkToBeginning[A, D](link: Link[A, D])(implicit proof: D => In, typeIntoOfA: TypeTagTree[Into[A]], typeIntoOfIn: TypeTagTree[Into[In]]): Runner[A, Out, State, Into] = {
    println(chain.asDefaultString)
//Need to use *proxy* everywhere!!!!!!!
    val originalRunner: In => Out =
      originalLink.toFunction

    val linkRunner: A => D =
      link.toFunction

    val newMappedLink: Link[A, Out] =
      link ~> originalLink

    val fnNewMappedLink: A => Out =
      newMappedLink.toFunction

    val newRunLink: Link[A, Into[Out]] =
      Link(ops.liftRunner(newMappedLink.chain, newMappedLink.runner)(state, newMappedLink.typeIn, newMappedLink.typeOut))(newMappedLink.typeIn, typeOut)

    val newRunner: A => Into[Out] =
      newRunLink.toFunction

    val newChainLink: Link[Into[A], Into[In]] =
      Link((first: Into[A]) => {
        val second: Into[D] = ops.map(first)(linkRunner)
        val third: Into[In] = ops.map(second)(proof)
        third
      })(typeIntoOfA, typeIntoOfIn)

    val liftedChainLink: ChainLink = newChainLink +: chain //Prepend this link
    val liftedRunChain: ChainRunner[Into] = runChain //No change to list of Runner (lift) instances

    Runner.withLink(name, liftedChainLink, newRunLink, newMappedLink, liftedRunChain, state, ops, rewireOnFlatMap = false, mapStateOnRun _)(typeState, link.typeIn, typeOut)
  }

  def map[A, D, S, F[_]](fn: Runner[In, Out, State, Into] => Runner[A, D, S, F]): Runner[A, D, S, F] =
    fn(this)

  def asShortString: String =
    null

  val asDefaultString = {
    val in = typeIn.toShortString
    val out = typeOut.toShortString

    s"$name[$in, $out]"
  }

  val toShortString = {
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

  private case class Build[A, D, S, F[_]](name: String, chain: ChainLink, link: Link[A, F[D]], originalLink: Link[A, D], providedChainRunner: ChainRunner[F], state: S, ops: RunnerOps[S, F], override val rewireOnFlatMap: Boolean, mapStateOnRun: (S, ChainableRunner[F]) => S, override val asShortString: String = null)(implicit val typeState: TypeTagTree[S], val typeIn: TypeTagTree[A], val typeOut: TypeTagTree[F[D]]) extends Runner[A, D, S, F] with Named {
    val runner =
      link.runner

    lazy val runChain: ChainRunner[F] =
      if ((providedChainRunner eq null) || providedChainRunner.isEmpty)
        immutable.Vector(this.asInstanceOf[ChainableRunner[F]])
      else
        providedChainRunner
  }

  private[fluxmuster5] def liftLink[A, D, S, F[_]](chained: Chained[A, D], state: S, ops: RunnerOps[S, F])(implicit tOut: TypeTagTree[F[D]]): Link[A, F[D]] = {
    val chain = chained.chain
    val runner = chained.runner
    val liftedRunner = Link(ops.liftRunner[A, D](chain, runner)(state, chained.typeIn, chained.typeOut))(chained.typeIn, tOut)
    liftedRunner
  }

  def withLink[A, D, S, F[_]](name: String, chain: ChainLink, link: Link[A, F[D]], originalLink: Link[A, D], runChain: ChainRunner[F], state: S, ops: RunnerOps[S, F], rewireOnFlatMap: Boolean = false, mapStateOnRun: (S, ChainableRunner[F]) => S = (s: S, _: ChainableRunner[F]) => s)(implicit tState: TypeTagTree[S], tIn: TypeTagTree[A], tOut: TypeTagTree[F[D]]): Runner[A, D, S, F] =
    Build[A, D, S, F](name, chain, link, originalLink, runChain, state, ops, rewireOnFlatMap, mapStateOnRun)(tState, tIn, tOut)

  def withUnliftedLink[A, D, S, F[_]](name: String, link: Link[A, D], runChain: ChainRunner[F], state: S, ops: RunnerOps[S, F], rewireOnFlatMap: Boolean = false, mapStateOnRun: (S, ChainableRunner[F]) => S = (s: S, _: ChainableRunner[F]) => s)(implicit typeState: TypeTagTree[S], typeIn: TypeTagTree[A], typeOut: TypeTagTree[F[D]]): Runner[A, D, S, F] = {
    val liftedRunner = liftLink(link, state, ops)
    val liftedChain = immutable.Vector(liftedRunner)
    val lifted = withLink[A, D, S, F](name, liftedChain, liftedRunner, link, runChain, state, ops, rewireOnFlatMap, mapStateOnRun)
    lifted
  }
}