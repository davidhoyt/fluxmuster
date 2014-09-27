package com.github.davidhoyt.fluxmuster.lift


import akka.actor._
import akka.util.Timeout
import com.github.davidhoyt.fluxmuster.Chains.LinkChain
import com.github.davidhoyt.fluxmuster._
import scala.concurrent.ExecutionContext

import scala.util.{Failure, Success, Try}

case class AkkaConfig(implicit val timeout: Timeout, val context: ExecutionContext, val actorRefFactory: ActorRefFactory)

object Akka {
  import akka.pattern.ask
  import scala.concurrent.Future

  val defaultName =
    Macros.simpleNameOf[Akka.type]

  def apply[A, D](config: AkkaConfig): PartialLift[A, D, AkkaConfig, Future] =
    apply(defaultName, config)

  def apply[A, D](name: String, config: AkkaConfig): PartialLift[A, D, AkkaConfig, Future] =
    PartialLift(name, config, AkkaSerialOps)

  def par[A, D](config: AkkaConfig): PartialLift[A, D, AkkaConfig, Future] =
    par(defaultName, config)

  def par[A, D](name: String, config: AkkaConfig): PartialLift[A, D, AkkaConfig, Future] =
    PartialLift(name, config, AkkaParallelOps)

  object AkkaSerialOps extends LiftOps[AkkaConfig, Future] {
    def point[A](given: => A): Future[A] =
      FutureLiftOps.point(given)

    def flatten[A](given: Future[Future[A]])(implicit config: AkkaConfig): Future[A] =
      FutureLiftOps.flatten(given)(config.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit config: AkkaConfig): Future[B] =
      FutureLiftOps.map(given)(fn)(config.context)

    def liftRunner[A, D](linkChain: LinkChain, opsChain: ChainedLiftOps[Future], runner: A => D)(implicit config: AkkaConfig, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] =
      runSerial(runner, linkChain, config)
  }

  object AkkaParallelOps extends LiftOps[AkkaConfig, Future] {
    def point[A](given: => A): Future[A] =
      FutureLiftOps.point(given)

    def flatten[A](given: Future[Future[A]])(implicit config: AkkaConfig): Future[A] =
      FutureLiftOps.flatten(given)(config.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit config: AkkaConfig): Future[B] =
      FutureLiftOps.map(given)(fn)(config.context)

    def liftRunner[A, D](linkChain: LinkChain, opsChain: ChainedLiftOps[Future], runner: A => D)(implicit config: AkkaConfig, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] =
      runParallel(runner, linkChain, config)
  }

  private def runSerial[A, D](runner: A => D, linkChain: LinkChain, config: AkkaConfig)(a: A): Future[D] = {
    import config._

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

  private case class Run[A, D](value: A, linkChain: LinkChain, runner: A => D)
  private case class Response[D](value: Try[D])

  private class RunnerActor[A, D] extends Actor with ActorLogging {
    def receive: Receive = {
      case Run(value, foo: LinkChain, runner) =>
        val v = value.asInstanceOf[A]
        val r = runner.asInstanceOf[A => D]

        sender() ! Response(Try(r(v)))
        //sender() ! Response(Try(foo.runLinkChainAny(v)))

        context.stop(self)

      case other =>
        log.warning("Unrecognized message: {}", other)
    }
  }

  private case class Initialize(next: Option[ActorRef])
  private case class Next[A](value: Try[A], originator: Option[ActorRef])

  private def runParallel[A, D](implicit runner: A => D, linkChain: LinkChain, config: AkkaConfig): A => Future[D] = {
    import akka.pattern.ask
    import config._

    //TODO: Needs a parent actor? Perhaps not since the ActorRefFactory can be the parent actor's context?

    require(linkChain.nonEmpty, s"Missing a fluxmuster proxy")

    //Create the actors only when they're actually asked for but then
    //effectively leave them running.
    lazy val actorRefs = {
      val createdActorRefs =
        for (link <- linkChain)
          yield actorRefFactory.actorOf(Props(new ParallelProxyActor(link)))

      //Link them together.
      Utils.prevCurrentNext(createdActorRefs) {
        case (_, current, next) =>
          current ! Initialize(next)
      }

      createdActorRefs
    }

    (a: A) => {
      val run =
        (actorRefs.head ? Next[A](Success(a), None))
        .map {
          case Success(value) =>
            value.asInstanceOf[D]
          case Failure(error) =>
            throw error
          case unexpected =>
            throw new IllegalStateException(s"Received unexpected value: $unexpected")
        }

      run onFailure {
        case error =>
          throw error
      }

      run
    }
  }

  class ParallelProxyActor[In, Out](provided: LinkAny) extends Actor with ActorLogging {
    import akka.actor.SupervisorStrategy._
    import scala.util.control.NonFatal

    val link =
      provided.asInstanceOf[Link[In, Out]]

    var nextRef: Option[ActorRef] =
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
      case msg @ Initialize(next) =>
        log.info("Received initialization for: {}", msg)
        nextRef = next
        context.become(readyForProcessing)

      case unexpected =>
        log.warning("Unexpectedly received message during initialization of parallel proxy actor: {}", unexpected)
    }

    def readyForProcessing: Receive = {
      case msg @ Next(value: Try[In], originator) =>
        log.info("Received next for: {}", msg)

        //Get the sender here since Initialize() messages aren't
        //sent by the temporary actor created for completing the future.
        val recipient = originator orElse Some(sender())
        val result = value map link.run

        if (nextRef.isDefined)
          nextRef.get ! Next(result, recipient)
        else
          recipient.get ! result

      case unexpected =>
        log.warning("Unexpectedly received message during processing of parallel proxy actor: {}", unexpected)
    }
  }
}
