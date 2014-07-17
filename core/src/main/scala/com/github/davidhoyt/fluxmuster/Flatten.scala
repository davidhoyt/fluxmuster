package com.github.davidhoyt.fluxmuster


object Flatten {
  import scala.concurrent.{ExecutionContext, Future}

  val NAME = Macros.nameOf[Flatten.type]

  def apply[A, B, C](implicit context: ExecutionContext): ProxySpecification[A, A, Future[Future[C]], Future[C]] =
    apply(NAME)

  def apply[A, B, C](name: String)(implicit context: ExecutionContext): ProxySpecification[A, A, Future[Future[C]], Future[C]] =
    ProxySpecification(name)(identity, flattenFuture(context))

  def flattenFuture[C](executor: ExecutionContext)(c: Future[Future[C]]): Future[C] =
    c.flatMap(identity)(executor)
}
