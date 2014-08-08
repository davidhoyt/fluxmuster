package com.github.davidhoyt.fluxmuster2.lift

import akka.actor._
import akka.util.Timeout

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}
import com.github.davidhoyt.fluxmuster2._

case class AkkaConfiguration(name: String = Macros.nameOf[Akka.type])

object Akka {
  import scala.concurrent.{Future, ExecutionContext}
  import scala.util.{Try, Success, Failure}

  val NAME = Macros.simpleNameOf[Akka.type]

  case class Run[A, D](value: A, runner: A => D)

  case class Response[D](value: Try[D])

  case class State(configuration: AkkaConfiguration)(implicit val timeout: Timeout, val context: ExecutionContext, val actorRefFactory: ActorRefFactory)

  def apply[T](implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): LiftedNeedsStep[State, Future] =
    apply(AkkaConfiguration())

  def apply[T](configuration: AkkaConfiguration)(implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): LiftedNeedsStep[State, Future] =
    LiftedNeedsStep(NAME, State(configuration), AkkaSerialOps)

  def par[T](configuration: AkkaConfiguration)(implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): LiftedNeedsStep[State, Future] =
    LiftedNeedsStep(NAME, State(configuration), AkkaParallelOps)

  object AkkaSerialOps extends LiftOp[State, Future] {
    implicit def point[A](given: => A)(implicit state: State): Future[A] =
      FutureLiftOp.point(given)(state.context)

    implicit def flatten[A](given: Future[Future[A]])(implicit state: State): Future[A] =
      FutureLiftOp.flatten(given)(state.context)

    implicit def map[A, B](given: Future[A])(fn: A => B)(implicit state: State): Future[B] =
      FutureLiftOp.map(given)(fn)(state.context)

    implicit def apply[A, D](runner: A => D)(implicit state: State, connections: Connections, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => Future[D] =
      runSerial(runner, state)
  }

  object AkkaParallelOps extends LiftOp[State, Future] {
    implicit def point[A](given: => A)(implicit state: State): Future[A] =
      FutureLiftOp.point(given)(state.context)

    implicit def flatten[A](given: Future[Future[A]])(implicit state: State): Future[A] =
      FutureLiftOp.flatten(given)(state.context)

    implicit def map[A, B](given: Future[A])(fn: A => B)(implicit state: State): Future[B] =
      FutureLiftOp.map(given)(fn)(state.context)

    implicit def apply[A, D](runner: A => D)(implicit state: State, connections: Connections, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => Future[D] =
      runParallel(runner, connections, state)
  }

  private def runSerial[A, D](runner: A => D, state: State)(a: A): Future[D] = {
    import akka.pattern.ask

    import state._

    val actor = actorRefFactory.actorOf(Props[RunnerActor[A, D]])
    val future = (actor ? Run[A, D](a, runner)) map { value =>
      value.asInstanceOf[Response[D]] match {
        case Response(Success(result)) =>
          result.asInstanceOf[D]
        case Response(Failure(err)) =>
          throw err
      }
    }

    future.onComplete { _ =>
      actorRefFactory.stop(actor)
    }

    future
  }

  object RunnerActor

  class RunnerActor[A, D] extends Actor with ActorLogging {
    def receive: Receive = {
      case Run(value, runner) =>
        val v = value.asInstanceOf[A]
        val r = runner.asInstanceOf[A => D]

        sender() ! Response(Try(r(v)))
      case other =>
        log.warning("Unrecognized message: {}", other)
    }
  }

  case class Initialize(prev: Option[ActorRef], next: Option[ActorRef])
  case class Downstream[A](recipient: Option[ActorRef], value: Try[A])
  case class Upstream[A](recipient: ActorRef, value: Try[A])

  private def runParallel[A, D](implicit runner: A => D, connections: Connections, state: State): A => Future[D] = {
    import akka.pattern.ask
    import state._

    //TODO: Needs a parent actor? Perhaps not since the ActorRefFactory can be the parent actor's context?

    require(connections.nonEmpty, s"Missing a fluxmuster step to run")

    //Create the actors only when they're actually asked for but then
    //effectively leave them running.
    lazy val actorRefs = {
      val createdActorRefs =
        for (step <- connections)
          yield actorRefFactory.actorOf(Props(new ParallelProxyActor(step, step.typeAcceptDownstream, step.typeMappedDownstream, step.typeAcceptUpstream, step.typeMappedUpstream)))

      //Link them together.
      prevCurrentNext(createdActorRefs) {
        case (prev, current, next) =>
          current ! Initialize(prev, next)
      }

      createdActorRefs
    }

    (a: A) => {
      val run =
        (actorRefs.head ? Downstream[A](None, Success(a)))
        .map {
          case Success(value) =>
            value.asInstanceOf[D]
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

  class ParallelProxyActor[A, B, C, D](provided: SimpleStep[_, _, _, _], tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]) extends Actor with ActorLogging {
    import akka.actor.SupervisorStrategy._
    import scala.util.control.NonFatal

    val step =
      provided.asInstanceOf[SimpleStep[A, B, C, D]]

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
        log.info("Received initialization for: {}", msg)
        downstreamRef = next
        upstreamRef = prev
        context.become(readyForProcessing)

      case unexpected =>
        log.warning("Unexpectedly received message during initialization of parallel proxy actor: {}", unexpected)
    }

    def readyForProcessing: Receive = {
      case msg @ Downstream(possibleRecipient, value: Try[A]) =>
        log.info("Received downstream for: {}", msg)

        //Get the sender here since Initialize() messages aren't
        //sent by the temporary actor created for completing the future.
        val recipient = possibleRecipient orElse Some(sender())
        val result = value map step.downstream

        if (downstreamRef.isDefined) {
          val downstream = downstreamRef.get
          downstream ! Downstream(recipient, result)
//        } else if (upstreamRef.isDefined) {
//          //Nothing to do but ship it on back.
//          upstreamRef map (_ ! Upstream(recipient.get, result))
//          //context.stop(self)
        } else {
          self ! Upstream(recipient.get, result)
        }

      case msg @ Upstream(recipient, value: Try[C]) =>
        log.info("Received upstream for {}: {}", msg)
        val response = value map step.upstream

        if (upstreamRef.isDefined)
          upstreamRef.get ! Upstream(recipient, response)
        else
          recipient ! response

        //context.stop(self)

      case unexpected =>
        log.warning("Unexpectedly received message during processing of parallel proxy actor: {}", unexpected)
    }
  }
}
