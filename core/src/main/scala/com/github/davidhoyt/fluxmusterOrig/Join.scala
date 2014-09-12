package com.github.davidhoyt.fluxmusterOrig

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

object Join {
  import scala.concurrent.{ExecutionContext, Future}

  val NAME = Macros.nameOf[Join.type]

  def apply[A, C](implicit context: ExecutionContext, tA: TypeTagTree[A], tC: TypeTagTree[Future[Future[C]]], tD: TypeTagTree[Future[C]]): ProxyStep[A, A, Future[Future[C]], Future[C]] =
    apply(NAME)(context, tA, tC, tD)

  def apply[A, C](name: String)(implicit context: ExecutionContext, tA: TypeTagTree[A], tC: TypeTagTree[Future[Future[C]]], tD: TypeTagTree[Future[C]]): ProxyStep[A, A, Future[Future[C]], Future[C]] =
    ProxyStep(Metadata(name, tA, tA, tC, tD))(identity, joinFuture(context))

  def joinFuture[C](executor: ExecutionContext)(c: Future[Future[C]]): Future[C] =
    c.flatMap(identity)(executor)
}
