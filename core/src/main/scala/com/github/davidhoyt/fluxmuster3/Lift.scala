package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

import scala.language.higherKinds

object LiftRun {
  type Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into[_]] =
    LiftLike[State, Into] with LiftChain[DownstreamIn, DownstreamOut, UpstreamIn, Into[UpstreamOut]] with Named
}

trait LiftRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into[_]]
  extends ChainRun[DownstreamIn, DownstreamOut, UpstreamIn, Into[UpstreamOut]] {
  self: LiftRun.Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into] =>

  //val proofDownstreamCanRouteToUpstream: DownstreamOut => UpstreamIn

  private[this] val convertIdentity = IdentityConverter[Into]

  def apply(in: DownstreamIn)(implicit tIn: TypeTagTree[DownstreamIn], tFrom: TypeTagTree[Into[UpstreamOut]]): Into[UpstreamOut] = {
    val foo = runInThisContext[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, Into](chain)(runner)(convertIdentity, tIn, tFrom, tFrom).apply(in)
    foo
  }

//  def apply[E, F >: In, G <: UpstreamIn](e: E)(implicit addlProofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
//    run(e)

  //def run[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit addlProofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
  //  upstream(addlProofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity)))

  /** Generates a [[Link]] instance that when called runs the downstream and then passes it to the upstream. */
  implicit def toLink: Link[DownstreamIn, Into[UpstreamOut]] =
    ???
    //createLink[DownstreamIn, DownstreamOut, UpstreamIn](proofDownstreamCanRouteToUpstream, identity, downstream.typeIn)

  /** Generates a [[Link]] instance that when called runs the downstream and then passes it to the upstream. */
  //implicit def createLink[E, F >: DownstreamOut, G <: UpstreamIn](implicit addlProofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn, tE: TypeTagTree[E]): Link[E, UpstreamOut] =
  //  Link((e: E) => upstream(addlProofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity))))(tE, upstream.typeOut)

  def routeDownToUp(in: DownstreamOut): UpstreamIn =
    runner.routeDownToUp(in)
    //proofDownstreamCanRouteToUpstream(in)

  def runDownChain(in: DownstreamIn): DownstreamOut =
    runner.runDownChain(in)
    //downstream(in)

  def runUpChain(in: UpstreamIn): Into[UpstreamOut] =
    runner.runUpChain(in)
    //upstream(in)
}

trait LiftDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into[_]] {

}

object Lift {
  private case class Builder[A, B, C, D, S, F[_]](name: String, chain: ChainLift, runner: ChainRun[A, B, C, F[D]], state: S, op: LiftOps[S, F])(implicit val typeState: TypeTagTree[S]) extends Lift[A, B, C, D, S, F] with Named

  def create[A, B, C, D, S, F[_]](name: String)(chain: ChainLift)(runner: ChainRun[A, B, C, F[D]], state: S, op: LiftOps[S, F])(implicit tState: TypeTagTree[S]): Lift[A, B, C, D, S, F] =
    Builder[A, B, C, D, S, F](name, chain, runner, state, op)(tState)
}

trait Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into[_]]
  extends LiftLike[State, Into]
  with LiftDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into]
  with LiftChain[DownstreamIn, DownstreamOut, UpstreamIn, Into[UpstreamOut]]
  with LiftRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, Into] {
  self: Named =>

}

object Async {
  import scala.collection.immutable
  import scala.concurrent.{ExecutionContext, Future}

  val NAME = Macros.simpleNameOf[Async.type]

  def apply[A, B, C, D](step: Step[A, B, C, D])(implicit context: ExecutionContext, typeState: TypeTagTree[ExecutionContext], typeResult: TypeTagTree[Future[D]]): ProjectedLift[A, D, ExecutionContext, Future] = {
    val f = FutureLiftOps.liftRunner[A, B, C, D](step)(context, step.chain, step.downstream.typeIn, step.upstream.typeOut)
    Lift.create(NAME)(immutable.Seq(step))(f.toLink(step.downstream.typeIn, typeResult), context, FutureLiftOps)(typeState)
  }
}

//trait Lift[In, Out, State, To[_]] extends LiftLike[State, To] with StepLike[In, To[Out], To[Out], To[Out]] with Runner[In, To[Out], To[Out], To[Out], LinkDownstream, LinkUpstream] with StepConnections {
//  import scala.collection._
//
//  def |>[W, G[_]](other: LiftedNeedsStep[W, G])(implicit converter: F -> G, tA: TypeTagTree[A], tW: TypeTagTree[W], tFofD: TypeTagTree[F[D]], tGofD: TypeTagTree[G[D]]): LiftedAndFlatten[A, D, W, G] =
//    lift(other)(converter, tA, tW, tFofD, tGofD)
//
//  def lift[W, G[_]](other: LiftedNeedsStep[W, G])(implicit converter: F -> G, tA: TypeTagTree[A], tW: TypeTagTree[W], tFofD: TypeTagTree[F[D]], tGofD: TypeTagTree[G[D]]): LiftedAndFlatten[A, D, W, G] =
//    other.lift(LiftedAndFlatten.this)(converter, tA, tGofD, tW, tFofD)
//
//  def |>[OtherIn >: In, OtherOut <: Out, OtherState, Other[_], OtherDownstream <: Downstream[_ >: OtherIn, _ <: Other[OtherOut]], OtherUpstream <: Upstream[_ <: Other[OtherIn], _ >: Other[OtherOut]]](other: Lift[OtherIn, OtherOut, OtherState, Other, OtherDownstream, OtherUpstream])(implicit converter: Other -> To, tIn: TypeTagTree[In], tToOut: TypeTagTree[To[Out]], tState: TypeTagTree[State], tOtherOut: TypeTagTree[Other[Out]]): Lift[In, Out, State, To, LinkDownstream, LinkUpstream] =
//    lift(other)(converter, tIn, tToOut, tState, tOtherOut)
//
//  def lift[OtherIn >: In, OtherOut <: Out, OtherState, Other[_], OtherDownstream <: Downstream[_ >: OtherIn, _ <: Other[OtherOut]], OtherUpstream <: Upstream[_ <: Other[OtherIn], _ >: Other[OtherOut]]](other: Lift[OtherIn, OtherOut, OtherState, Other, OtherDownstream, OtherUpstream])(implicit converter: Other -> To, tIn: TypeTagTree[In], tToOut: TypeTagTree[To[Out]], tState: TypeTagTree[State], tOtherOut: TypeTagTree[Other[Out]]): Lift[In, Out, State, To, LinkDownstream, LinkUpstream] = {
//    val connections = immutable.Seq(other)
//    Lift(name, runInThisContext(connections)(other.run)(converter, tIn, tOtherOut), state, op, connections)(tIn, tToOut, tState)
//  }
//}
//
//object Lift {
////  private case class Builder[A, D, S, F[_]](name: String, downstream: Downstream[A, F[D]], upstream: Upstream[F[D], F[D]], state: S, op: LiftOps[S, F], connections: Connections)(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedUpstream: TypeTagTree[F[D]], val typeState: TypeTagTree[S]) extends Lift[A, D, S, F, Downstream[A, F[D]], Upstream[F[D], F[D]]]
//
////  def apply[A, D, S, F[_]](name: String, downstream: Downstream[A, F[D]], state: S, op: LiftOps[S, F], connections: Connections)(implicit tA: TypeTagTree[A], tFofD: TypeTagTree[F[D]], tS: TypeTagTree[S]): Lift[A, D, S, F, Downstream[A, F[D]], Upstream[F[D], F[D]]] =
////    Builder[A, D, S, F](name, downstream, identity[F[D]], state, op, connections)(tA, tFofD, tS)
//}
//



trait LiftOps[State, Into[_]] {
  import scala.language.implicitConversions

  implicit def liftRunner[A, B, C, D](runner: ChainRun[A, B, C, D])(implicit state: State, chain: ChainLift, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => Into[D]
  implicit def point[A](given: => A)(implicit state: State): Into[A]
  implicit def flatten[A](given: Into[Into[A]])(implicit state: State): Into[A]
  implicit def map[A, B](given: Into[A])(fn: A => B)(implicit state: State): Into[B]
}

trait LiftChain[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  val chain: ChainLift
  val runner: ChainRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
}

trait LiftLike[State, Into[_]] {
  implicit val typeState: TypeTagTree[State]
  implicit val state: State
  implicit val op: LiftOps[State, Into]

  def runInThisContext[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, From[_]](chain: ChainLift)(runner: ChainRun[DownstreamIn, DownstreamOut, UpstreamIn, From[UpstreamOut]])(implicit converter: From -> Into, typeAccept: TypeTagTree[DownstreamIn], typeResult: TypeTagTree[From[UpstreamOut]], typeOut: TypeTagTree[Into[UpstreamOut]]): Downstream[DownstreamIn, Into[UpstreamOut]] = {
    //Pre-generate and reuse the runner as a function.
    val fnRunner = runner.runChain _
    Link((in: DownstreamIn) => {
      val runOtherInThisContext: DownstreamIn => Into[From[UpstreamOut]] = op.liftRunner(fnRunner)(state, chain, typeAccept, typeResult)
      val resultAfterRunning: Into[From[UpstreamOut]] = runOtherInThisContext(in)

      //flatMap!
      val mapResultBackIntoThisContext = op.map(resultAfterRunning)(converter.apply)(state)
      val flattenedBackIntoThisContext: Into[UpstreamOut] = op.flatten(mapResultBackIntoThisContext)(state)
      flattenedBackIntoThisContext
    })
  }
}
