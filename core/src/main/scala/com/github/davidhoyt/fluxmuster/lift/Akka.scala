package com.github.davidhoyt.fluxmuster.lift


import akka.actor._
import akka.util.Timeout
import com.github.davidhoyt.fluxmuster.Chains.LinkChain
import com.github.davidhoyt.fluxmuster._

import scala.util.{Failure, Success, Try}

case class AkkaConfig(par: Boolean = false, implicit val timeout: Timeout)

object Akka {
  import akka.actor.ActorRefFactory
  import akka.util.Timeout
  import scala.concurrent.{ExecutionContext, Future}

  val defaultName =
    Macros.simpleNameOf[Akka.type]

  case class State(config: AkkaConfig)(implicit val context: ExecutionContext, val actorRefFactory: ActorRefFactory)

  def apply[A, D](name: String = defaultName, config: AkkaConfig)
                 (implicit executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): PartialLift[A, D, State, Future] =
    PartialLift(name, State(config), if (!config.par) AkkaSerialOps else AkkaParallelOps)

//  def par[A, D](name: String = defaultName, config: AkkaConfig)
//               (implicit executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): PartialLift[A, D, State, Future] =
//    PartialLift(name, State(config), AkkaParallelOps)

  object AkkaSerialOps extends LiftOps[State, Future] {
    def point[A](given: => A): Future[A] =
      FutureLiftOps.point(given)

    def flatten[A](given: Future[Future[A]])(implicit state: State): Future[A] =
      FutureLiftOps.flatten(given)(state.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit state: State): Future[B] =
      FutureLiftOps.map(given)(fn)(state.context)

    def liftRunner[A, D](linkChain: LinkChain, opsChain: ChainedLiftOps[Future], runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] =
      runSerial(runner, linkChain, state)
  }

  object AkkaParallelOps extends LiftOps[State, Future] {
    def point[A](given: => A): Future[A] =
      FutureLiftOps.point(given)

    def flatten[A](given: Future[Future[A]])(implicit state: State): Future[A] =
      FutureLiftOps.flatten(given)(state.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit state: State): Future[B] =
      FutureLiftOps.map(given)(fn)(state.context)

    def liftRunner[A, D](linkChain: LinkChain, opsChain: ChainedLiftOps[Future], runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] =
      ???
      //runParallel(runner, linkChain, state)
  }

  private def runSerial[A, D](runner: A => D, linkChain: LinkChain, state: State)(a: A): Future[D] = {
    import akka.pattern.ask

    import state._
    import state.config._

    val actor = actorRefFactory.actorOf(Props[RunnerActor[A, D]])
    val future = (actor ? Run[A, D](a, linkChain, runner)) map { value =>
      value.asInstanceOf[Response[D]] match {
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

  object RunnerActor

  class RunnerActor[A, D] extends Actor with ActorLogging {
    def receive: Receive = {
      case Run(value, foo: LinkChain, runner) =>
        val v = value.asInstanceOf[A]
        val r = runner.asInstanceOf[A => D]


        sender() ! Response(Try(r(v)))
        //println(foo.asDefaultString)
        //sender() ! Response(Try(foo.runLinkChainAny(v)))

        context.stop(self)

      case other =>
        log.warning("Unrecognized message: {}", other)
    }
  }

  case class Run[A, D](value: A, linkChain: LinkChain, runner: A => D)
  case class Response[D](value: Try[D])

  case class Initialize(prev: Option[ActorRef], next: Option[ActorRef])
  case class Downstream[A](recipient: Option[ActorRef], value: Try[A])
  case class Upstream[A](recipient: ActorRef, value: Try[A])

//  private def runParallel[A, D](implicit runner: A => D, connections: LinkChain, state: State): A => Future[D] = {
//    import akka.pattern.ask
//    import state._
//
//    //TODO: Needs a parent actor? Perhaps not since the ActorRefFactory can be the parent actor's context?
//
//    require(connections.nonEmpty, s"Missing a fluxmuster step to run")
//
//    //Create the actors only when they're actually asked for but then
//    //effectively leave them running.
//    lazy val actorRefs = {
//      val createdActorRefs =
//        for (step <- connections)
//          yield actorRefFactory.actorOf(Props(new ParallelProxyActor(step)(step., step.typeMappedDownstream, step.typeAcceptUpstream, step.typeMappedUpstream)))
//
//      //Link them together.
//      Utils.prevCurrentNext(createdActorRefs) {
//        case (prev, current, next) =>
//          current ! Initialize(prev, next)
//      }
//
//      createdActorRefs
//    }
//
//    (a: A) => {
//      val run =
//        (actorRefs.head ? Downstream[A](None, Success(a)))
//        .map {
//          case Success(value) =>
//            value.asInstanceOf[D]
//          case Failure(error) =>
//            throw error
//          case unexpected =>
//            throw new IllegalStateException(s"Received unexpected value: $unexpected")
//        }
//
//      run.onFailure {
//        case error =>
//          throw error
//      }
//
//      run
//    }
//  }
//
//  class ParallelProxyActor[A, B, C, D](provided: ProxyAny)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]) extends Actor with ActorLogging {
//    import akka.actor.SupervisorStrategy._
//    import scala.util.control.NonFatal
//
//    val step =
//      provided.asInstanceOf[Proxy[A, B, C, D]]
//
//    var downstreamRef: Option[ActorRef] =
//      None
//
//    var upstreamRef: Option[ActorRef] =
//      None
//
//    //TODO: Provide retries via configuration. Also should always escalate to a parent actor.
//    //      Reconsider splitting out into creating new actors on each run instead of reusing
//    //      existing ones. If the decision is to keep it the way it is, provide a way to get
//    //      actor names so that a router can be defined in configuration for each step.
//    override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy(3) {
//      case NonFatal(error) => Resume
//      case _ => Escalate
//    }
//
//    def receive: Receive = {
//      case msg @ Initialize(prev, next) =>
//        log.info("Received initialization for: {}", msg)
//        downstreamRef = next
//        upstreamRef = prev
//        context.become(readyForProcessing)
//
//      case unexpected =>
//        log.warning("Unexpectedly received message during initialization of parallel proxy actor: {}", unexpected)
//    }
//
//    def readyForProcessing: Receive = {
//      case msg @ Downstream(possibleRecipient, value: Try[A]) =>
//        log.info("Received downstream for: {}", msg)
//
//        //Get the sender here since Initialize() messages aren't
//        //sent by the temporary actor created for completing the future.
//        val recipient = possibleRecipient orElse Some(sender())
//        val result = value map step.downstream.toFunction
//
//        if (downstreamRef.isDefined) {
//          val downstream = downstreamRef.get
//          downstream ! Downstream(recipient, result)
////        } else if (upstreamRef.isDefined) {
////          //Nothing to do but ship it on back.
////          upstreamRef map (_ ! Upstream(recipient.get, result))
////          //context.stop(self)
//        } else {
//          self ! Upstream(recipient.get, result)
//        }
//
//      case msg @ Upstream(recipient, value: Try[C]) =>
//        log.info("Received upstream for {}: {}", msg)
//        val response = value map step.upstream.toFunction
//
//        if (upstreamRef.isDefined)
//          upstreamRef.get ! Upstream(recipient, response)
//        else
//          recipient ! response
//
//        //context.stop(self)
//
//      case unexpected =>
//        log.warning("Unexpectedly received message during processing of parallel proxy actor: {}", unexpected)
//    }
//  }
}
