package com.github.davidhoyt.fluxmuster4.lift

import com.github.davidhoyt.fluxmuster.{TypeTagTree, Macros}
import com.github.davidhoyt.fluxmuster4._

object Async {
  import scala.concurrent.{ExecutionContext, Future}

  val NAME = Macros.simpleNameOf[Async.type]

  def apply[A, D](chained: Chained[A, D])(implicit context: ExecutionContext, typeState: TypeTagTree[ExecutionContext], typeIn: TypeTagTree[A], typeResult: TypeTagTree[Future[D]]): Lift[A, D, ExecutionContext, Future] =
    Lift.withChained(NAME, chained, EmptyChainLift, context, FutureLiftOps)
}
