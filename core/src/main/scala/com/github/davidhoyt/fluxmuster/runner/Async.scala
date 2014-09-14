package com.github.davidhoyt.fluxmuster.runner

import com.github.davidhoyt.fluxmuster._

object Async {
  import scala.concurrent.{ExecutionContext, Future}

  import scala.language.higherKinds

  val NAME = Macros.simpleNameOf[Async.type]

  def apply[A, B, C, D, S, F[_], G[_]](name: String, runner: Runner[A, B, C, D, S, F, G])(implicit context: ExecutionContext, converter: G -> Future, typeGofD: TypeTagTree[G[D]], typeIntoOfD: TypeTagTree[Future[D]]): Runner[A, B, C, D, ExecutionContext, G, Future] =
    Runner.withRunner(name, runner, context, FutureRunnerOps, rewireOnFlatMap = true)

  def apply[A, B, C, D](name: String, proxy: Proxy[A, B, C, D])(implicit context: ExecutionContext, typeOut: TypeTagTree[Future[D]]): Runner[A, B, C, D, ExecutionContext, Future, Future] =
    Runner.withUnliftedProxy(name, proxy, EmptyChainRunner, context, FutureRunnerOps, rewireOnFlatMap = true)

  def apply[A, D](name: String, link: Link[A, D])(implicit context: ExecutionContext, typeOut: TypeTagTree[Future[D]]): Runner[A, A, D, D, ExecutionContext, Future, Future] =
    apply(name, link.toProxy)

}
