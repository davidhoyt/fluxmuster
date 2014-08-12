package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.TypeTagTree


//object StepLike {
//  type Dependencies = BiDirectional with Named
//}
//
//trait StepLike { self: StepLike.Dependencies =>
//  def asShortString: String =
//    null
//
//  val asDefaultString =
//    s"$name[${self.downstream.typeIn.toShortString}, ${self.downstream.typeOut.toShortString}, ${self.upstream.typeIn.toShortString}, ${self.upstream.typeOut.toShortString}]"
//
//  val toShortString = {
//    val short = asShortString
//    if (short ne null)
//      short
//    else
//      asDefaultString
//  }
//
//  override def toString =
//    toShortString
//}
//
//trait StepConnections {
//  //
//  val foo = BiDirectional
//              (identity[Int]_ ~> identity[Int]_)
//              (identity[Int]_ ~> identity[Int]_)
//}
//
//trait Step /*extends StepLike[A, B, C, D] with Runner[A, B, C, D, LinkDownstream, LinkUpstream] with StepConnections*/ {
//
//}

//trait MyUpstream[-In, +Out] {
//  def apply[A >: In, B <: Out](a: A): B
//}
//
//trait Runner[In, B, C, Out, LinkDownstream <: Downstream[_ >: In, _ <: B], LinkUpstream <: Upstream[_ >: C, _ <: Out]] { this: StepConnections =>
//  val downstream: LinkDownstream
//  val upstream: LinkUpstream
//
//  def apply[E, F >: B, G <: C](e: E)(implicit fToG: F => G, eToIn: E => In): Out =
//    run(e)
//
//  def run[E, F >: B, G <: C](e: E)(implicit fToG: F => G, eToIn: E => In): Out = {
//    val f = downstream.apply(eToIn(e))
//    upstream(fToG(downstream(eToIn(e))))
//  }
//}
//
//trait StepConnections {
//  val connections: Connections
//}
//
//trait StepLike[A, B, C, D] {
//  implicit val typeAcceptDownstream: TypeTagTree[A]
//  implicit val typeMappedDownstream: TypeTagTree[B]
//  implicit val typeAcceptUpstream: TypeTagTree[C]
//  implicit val typeMappedUpstream: TypeTagTree[D]
//
//  val name: String
//
//  def asShortString: String =
//    null
//
//  lazy val asDefaultString =
//    s"$name[${typeAcceptDownstream.toShortString}, ${typeMappedDownstream.toShortString}, ${typeAcceptUpstream.toShortString}, ${typeMappedUpstream.toShortString}]"
//
//  lazy val toShortString = {
//    val short = asShortString
//    if (short ne null)
//      short
//    else
//      asDefaultString
//  }
//
//  override def toString =
//    toShortString
//}
//
//trait Step[A, B, C, D, LinkDownstream <: Downstream[_ >: A, _ <: B], LinkUpstream <: Upstream[_ >: C, _ <: D]] extends StepLike[A, B, C, D] with Runner[A, B, C, D, LinkDownstream, LinkUpstream] with StepConnections {
//
//  def combine[B2 >: B, C2 <: C, T, U](step: SimpleStep[B2, T, U, C2])(implicit tA: TypeTagTree[A], tT: TypeTagTree[T], tU: TypeTagTree[U], tD: TypeTagTree[D]): Step[A, T, U, D, Downstream[A, T], Upstream[U, D]] = {
//    val down = downstream andThen step.downstream
//    val up = upstream compose step.upstream
//
//    val connections: Connections =
//      (Step.this.connections ++ step.connections).foldLeft(EmptyConnections) {
//        case (seq, p) if p.connections.nonEmpty =>
//          seq :+ p.connections.head
//        case (seq, _) =>
//          seq
//      }
//
//    Step("<~>", down, up, connections)(tA, tT, tU, tD)
//  }
//}
//
//object Step {
//  import scala.collection._
//
//  private case class Builder[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], providedConnections: Connections, override val asShortString: String = null)(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedDownstream: TypeTagTree[B], val typeAcceptUpstream: TypeTagTree[C], val typeMappedUpstream: TypeTagTree[D]) extends Step[A, B, C, D, Downstream[A, B], Upstream[C, D]] {
//    lazy val connections = {
//      if ((providedConnections eq null) || providedConnections.isEmpty)
//        immutable.Seq(this)
//      else
//        providedConnections
//    }
//  }
//
//  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D, Downstream[A, B], Upstream[C, D]] =
//    Builder(name, downstream, upstream, null)(tA, tB, tC, tD)
//
//  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], connections: Connections)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D, Downstream[A, B], Upstream[C, D]] =
//    Builder(name, downstream, upstream, connections)(tA, tB, tC, tD)
//
//  def unapply[A, B, C, D](step: SimpleStep[A, B, C, D]): Option[(Downstream[A, B], Upstream[C, D], Connections)] =
//    PartialFunction.condOpt(step) {
//      case _ => (step.downstream, step.upstream, step.connections)
//    }
//}
//
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

object Fluxmuster3 extends App {
  val link1: Link[Long, String] = (x: Long) => x.toString
  val link2: Link[String, Long] = (x: String) => x.toLong
  val link1To2 = link1 ~> link2 ~> link1 ~> link2 ~> link1
  val link2To1 = link2 <~ link1 <~ link2 <~ link1 <~ link2
  val result = link1To2(111)

  //Check chain when you include yourself more than once
  println(result)
  println(link1To2.chain)
  println(link2To1.chain)
}