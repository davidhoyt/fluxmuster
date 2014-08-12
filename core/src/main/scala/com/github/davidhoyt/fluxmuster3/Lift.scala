package com.github.davidhoyt.fluxmuster3

////trait Lift[In, Out, State, To[_]] extends LiftLike[State, To] with StepLike[In, To[Out], To[Out], To[Out]] with Runner[In, To[Out], To[Out], To[Out], LinkDownstream, LinkUpstream] with StepConnections {
////  import scala.collection._
////
////  type LinkDownstream <: Downstream[_ >: In, _ <: To[Out]]
////  type LinkUpstream <: Upstream[_ >: To[Out], _ <: To[Out]]
////
////  implicit lazy val typeMappedDownstream = typeMappedUpstream
////  implicit lazy val typeAcceptUpstream = typeMappedUpstream
//
////  def |>[W, G[_]](other: LiftedNeedsStep[W, G])(implicit converter: F -> G, tA: TypeTagTree[A], tW: TypeTagTree[W], tFofD: TypeTagTree[F[D]], tGofD: TypeTagTree[G[D]]): LiftedAndFlatten[A, D, W, G] =
////    lift(other)(converter, tA, tW, tFofD, tGofD)
////
////  def lift[W, G[_]](other: LiftedNeedsStep[W, G])(implicit converter: F -> G, tA: TypeTagTree[A], tW: TypeTagTree[W], tFofD: TypeTagTree[F[D]], tGofD: TypeTagTree[G[D]]): LiftedAndFlatten[A, D, W, G] =
////    other.lift(LiftedAndFlatten.this)(converter, tA, tGofD, tW, tFofD)
//
////  def |>[OtherIn >: In, OtherOut <: Out, OtherState, Other[_], OtherDownstream <: Downstream[_ >: OtherIn, _ <: Other[OtherOut]], OtherUpstream <: Upstream[_ <: Other[OtherIn], _ >: Other[OtherOut]]](other: Lift[OtherIn, OtherOut, OtherState, Other, OtherDownstream, OtherUpstream])(implicit converter: Other -> To, tIn: TypeTagTree[In], tToOut: TypeTagTree[To[Out]], tState: TypeTagTree[State], tOtherOut: TypeTagTree[Other[Out]]): Lift[In, Out, State, To, LinkDownstream, LinkUpstream] =
////    lift(other)(converter, tIn, tToOut, tState, tOtherOut)
////
////  def lift[OtherIn >: In, OtherOut <: Out, OtherState, Other[_], OtherDownstream <: Downstream[_ >: OtherIn, _ <: Other[OtherOut]], OtherUpstream <: Upstream[_ <: Other[OtherIn], _ >: Other[OtherOut]]](other: Lift[OtherIn, OtherOut, OtherState, Other, OtherDownstream, OtherUpstream])(implicit converter: Other -> To, tIn: TypeTagTree[In], tToOut: TypeTagTree[To[Out]], tState: TypeTagTree[State], tOtherOut: TypeTagTree[Other[Out]]): Lift[In, Out, State, To, LinkDownstream, LinkUpstream] = {
////    val connections = immutable.Seq(other)
////    Lift(name, runInThisContext(connections)(other.run)(converter, tIn, tOtherOut), state, op, connections)(tIn, tToOut, tState)
////  }
////}
//
//object Lift {
////  private case class Builder[A, D, S, F[_]](name: String, downstream: Downstream[A, F[D]], upstream: Upstream[F[D], F[D]], state: S, op: LiftOps[S, F], connections: Connections)(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedUpstream: TypeTagTree[F[D]], val typeState: TypeTagTree[S]) extends Lift[A, D, S, F, Downstream[A, F[D]], Upstream[F[D], F[D]]]
//
////  def apply[A, D, S, F[_]](name: String, downstream: Downstream[A, F[D]], state: S, op: LiftOps[S, F], connections: Connections)(implicit tA: TypeTagTree[A], tFofD: TypeTagTree[F[D]], tS: TypeTagTree[S]): Lift[A, D, S, F, Downstream[A, F[D]], Upstream[F[D], F[D]]] =
////    Builder[A, D, S, F](name, downstream, identity[F[D]], state, op, connections)(tA, tFofD, tS)
//}
//

trait Chainable {
}

//trait LiftOps[State, Into[_]] {
//  implicit def liftRunner[A, D](runner: A => D)(implicit state: State, connections: Connections, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => Into[D]
//  implicit def point[A](given: => A)(implicit state: State): Into[A]
//  implicit def flatten[A](given: Into[Into[A]])(implicit state: State): Into[A]
//  implicit def map[A, B](given: Into[A])(fn: A => B)(implicit state: State): Into[B]
//}
//
//trait LiftLike[State, Into[_]] {
//  implicit val typeState: TypeTagTree[State]
//  implicit val state: State
//  implicit val op: LiftOps[State, Into]
//
//  protected def runInThisContext[In, Out, From[_]](connections: Connections)(runner: In => From[Out])(implicit converter: From -> Into, typeAccept: TypeTagTree[In], typeResult: TypeTagTree[From[Out]]): Downstream[In, Into[Out]] =
//    (in: In) => {
//      val runOtherInThisContext: In => Into[From[Out]] = op.liftRunner(runner)(state, connections, typeAccept, typeResult)
//      val resultAfterRunning: Into[From[Out]] = runOtherInThisContext(in)
//
//      //flatMap!
//      val mapResultBackIntoThisContext = op.map(resultAfterRunning)(converter.apply)(state)
//      val flattenedBackIntoThisContext: Into[Out] = op.flatten(mapResultBackIntoThisContext)(state)
//      flattenedBackIntoThisContext
//    }
//}
