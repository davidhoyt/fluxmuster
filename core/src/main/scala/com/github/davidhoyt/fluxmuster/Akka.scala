package com.github.davidhoyt.fluxmuster

import akka.actor._
import akka.util.Timeout

case class AkkaConfiguration(name: String = Macros.nameOf[Akka.ProxyActor.type])

object Akka {
  import scala.concurrent.{Future, ExecutionContext}
  import scala.util.{Try, Success, Failure}

  case class Run[A, B, C, D](value: A, specification: ProxySpecification[A, B, C, D])
  case class Response[D](value: Try[D])

  def apply[A, B, C](implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory, tA: TypeTagTree[A], tC: TypeTagTree[Future[C]]): ProxyLift[A, B, B, C, A, Future[C], Future[C], Future[C]] =
    apply(AkkaConfiguration())(timeout, executionContext, actorRefFactory, tA, tC)

  def apply[A, B, C](configuration: AkkaConfiguration)(implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory, tA: TypeTagTree[A], tC: TypeTagTree[Future[C]]): ProxyLift[A, B, B, C, A, Future[C], Future[C], Future[C]] =
    (p2: ProxySpecification[A, B, B, C]) => {
      val downstream = run(p2, configuration, timeout, executionContext, actorRefFactory)_
      val upstream = identity[Future[C]]_
      ProxySpecification(Metadata(Macros.nameOf[Akka.type], tA, tC, tC, tC) +: p2.metadata, downstream, upstream, p2.connections)
    }

  def par[A, B, C](configuration: AkkaConfiguration)(implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory, tA: TypeTagTree[A], tC: TypeTagTree[Future[C]]): ProxyLift[A, B, B, C, A, Future[C], Future[C], Future[C]] = {
    (p2: ProxySpecification[A, B, B, C]) => {
      val downstream = runParallel(p2, configuration, timeout, executionContext, actorRefFactory)_
      val upstream = identity[Future[C]]_
      ProxySpecification(Metadata(Macros.nameOf[Akka.type], tA, tC, tC, tC) +: p2.metadata, downstream, upstream, p2.connections)
    }
  }

  private def run[A, B, C](specification: ProxySpecification[A, B, B, C], configuration: AkkaConfiguration, timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory)(a: A): Future[C] = {
    import akka.pattern.ask

    implicit val t = timeout
    implicit val ec = executionContext

    val actor = actorRefFactory.actorOf(Props[ProxyActor[A, B, C]])
    val future = (actor ? Run[A, B, B, C](a, specification)) map { value =>
      value.asInstanceOf[Response[C]] match {
        case Response(Success(result)) =>
          result
        case Response(Failure(err)) =>
          throw err
      }
    }

    future.onComplete { _ =>
      actorRefFactory.stop(actor)
    }

    future
  }

  object ProxyActor

  class ProxyActor[A, B, C] extends Actor with ActorLogging {
    def receive: Receive = {
      case Run(value, specification) =>
        val v = value.asInstanceOf[A]
        val spec = specification.asInstanceOf[ProxySpecification[A, B, B, C]]

        sender() ! Response(Try(ProxySpecification.run(spec)(v)))
    }
  }

  private def runParallel[A, B, C](specification: ProxySpecification[A, B, B, C], configuration: AkkaConfiguration, timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory)(a: A): Future[C] = {
    ???
  }
  class Proxy
}
