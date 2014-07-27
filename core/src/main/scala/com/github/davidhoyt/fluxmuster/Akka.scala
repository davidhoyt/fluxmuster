package com.github.davidhoyt.fluxmuster

import akka.actor._
import akka.util.Timeout

case class AkkaConfiguration(name: String = Macros.nameOf[Akka.ProxyActor.type])

object Akka {

  import scala.concurrent.{Future, ExecutionContext}
  import scala.util.{Try, Success, Failure}

  val NAME = Macros.simpleNameOf[Akka.type]

  case class Run[A, B, C, D](value: A, specification: ProxySpecification[A, B, C, D])

  case class Response[D](value: Try[D])

  def apply(implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): ProxyLiftDownstreamWithHint[Nothing, Future] =
    apply(AkkaConfiguration())

  def apply(configuration: AkkaConfiguration)(implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): ProxyLiftDownstreamWithHint[Nothing, Future] =
    new ProxyLiftDownstreamWithHint[Nothing, Future] {
      protected val name = NAME
      protected def downstream[A, B, C](spec: ProxySpecification[A, B, B, C])(implicit evidence: Nothing <:< C): LinkDownstream[A, Future[C]] =
        run(spec, configuration, timeout, executionContext, actorRefFactory) _
  }

  def par(configuration: AkkaConfiguration)(implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): ProxyLiftDownstreamWithHint[Nothing, Future] =
    new ProxyLiftDownstreamWithHint[Nothing, Future] {
      protected val name = NAME
      protected def downstream[A, B, C](spec: ProxySpecification[A, B, B, C])(implicit evidence: Nothing <:< C): LinkDownstream[A, Future[C]] =
        runParallel(spec, configuration, timeout, executionContext, actorRefFactory)
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
      case other =>
        log.warning("Unrecognized message: {}", other)
    }
  }

  case class Initialize(prev: Option[ActorRef], next: Option[ActorRef])
  case class Downstream[A](recipient: Option[ActorRef], value: Try[A])
  case class Upstream[A](recipient: ActorRef, value: Try[A])

  private def runParallel[A, B, C](specification: ProxySpecification[A, B, B, C], configuration: AkkaConfiguration, timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): A => Future[C] = {
    import akka.pattern.ask

    implicit val t = timeout
    implicit val ec = executionContext

    //TODO: Needs a parent actor.

    //Create the actors.
    val actorRefs =
      for (spec <- specification.connections)
      yield {
        val meta = spec.metadata.head
        actorRefFactory.actorOf(Props(new ParallelProxyActor(meta, spec, meta.typeAcceptDownstream, meta.typeMappedDownstream, meta.typeAcceptUpstream, meta.typeMappedUpstream)))
      }

    //Link them together.
    prevCurrentNext(actorRefs) {
      case (prev, current, next) =>
        current ! Initialize(prev, next)
    }

    (a: A) => {
      val run =
        (actorRefs.head ? Downstream[A](None, Success(a)))
        .map {
          case Success(value) =>
            value.asInstanceOf[C]
          case Failure(error) =>
            throw error
          case unexpected =>
            throw new IllegalStateException(s"Received unexpected value: $unexpected")
        }

      run.onFailure {
        case error =>
          throw error
      }

      run
    }
  }

  class ParallelProxyActor[A, B, C, D](metadata: Metadata, provided: ProxySpecification[_, _, _, _], tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]) extends Actor with ActorLogging {
    import akka.actor.SupervisorStrategy._
    import scala.util.control.NonFatal

    val spec =
      provided.asInstanceOf[ProxySpecification[A, B, C, D]]

    var downstreamRef: Option[ActorRef] =
      None

    var upstreamRef: Option[ActorRef] =
      None

    //TODO: Provide retries via configuration. Also should always escalate to a parent actor.
    //      Reconsider splitting out into creating new actors on each run instead of reusing
    //      existing ones. If the decision is to keep it the way it is, provide a way to get
    //      actor names so that a router can be defined in configuration for each step.
    override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy(3) {
      case NonFatal(error) => Resume
      case _ => Escalate
    }

    def receive: Receive = {
      case msg @ Initialize(prev, next) =>
        log.info("Received initialization for {}: {}", metadata, msg)
        downstreamRef = next
        upstreamRef = prev
        context.become(readyForProcessing)

      case unexpected =>
        log.warning("Unexpectedly received message during initialization of parallel proxy actor: {}", unexpected)
    }

    def readyForProcessing: Receive = {
      case msg @ Downstream(possibleRecipient, value: Try[A]) =>
        log.info("Received downstream for {}: {}", metadata, msg)

        //Get the sender here since Initialize() messages aren't
        //sent by the temporary actor created for completing the future.
        val recipient = possibleRecipient orElse Some(sender())
        val result = value map spec.downstream

        if (downstreamRef.isDefined) {
          val downstream = downstreamRef.get
          downstream ! Downstream(recipient, result)
        } else {
          //Nothing to do but ship it on back.
          upstreamRef map (_ ! Upstream(recipient.get, result))
          //context.stop(self)
        }

      case msg @ Upstream(recipient, value: Try[C]) =>
        log.info("Received upstream for {}: {}", metadata, msg)
        val response = value map spec.upstream

        if (upstreamRef.isDefined)
          upstreamRef.get ! Upstream(recipient, response)
        else
          recipient ! response

        //context.stop(self)

      case unexpected =>
        log.warning("Unexpectedly received message during processing of parallel proxy actor: {}", unexpected)
    }
  }
  class Proxy
}
