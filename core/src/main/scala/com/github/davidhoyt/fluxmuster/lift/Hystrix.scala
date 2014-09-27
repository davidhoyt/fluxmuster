package com.github.davidhoyt.fluxmuster.lift

import com.github.davidhoyt.fluxmuster._
import com.netflix.hystrix.HystrixCommand
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

case class HystrixConfig(group: String = "default", command: String = "default", builder: HystrixCommand.Setter => HystrixCommand.Setter = identity)(implicit val timeout: FiniteDuration = 1.second, val context: ExecutionContext)

object Hystrix {
  import com.netflix.hystrix.{HystrixCommandGroupKey, HystrixCommandKey, HystrixCommandProperties}
  import scala.concurrent.{Promise, Future}
  import Chains._

  val defaultName =
    Macros.simpleNameOf[Hystrix.type]

  case class State private[Hystrix] (fallback: Option[() => Any], config: HystrixConfig)
                                    (implicit val typeFallback: TypeTagTree[Any], val typeLiftedFallback: TypeTagTree[Future[Any]])

  def apply[A, D](config: HystrixConfig)
                 (implicit typeOut: TypeTagTree[Future[D]]): PartialLift[A, D, State, Future] =
    apply(defaultName, config)

  def apply[A, D](name: String, config: HystrixConfig)
                 (implicit typeOut: TypeTagTree[Future[D]]): PartialLift[A, D, State, Future] =
    PartialLift(name, State(None, config), HystrixLiftOps)

  def withFallback[A, D](config: HystrixConfig)
                        (fallback: => D)
                        (implicit typeOut: TypeTagTree[Future[D]]): PartialLift[A, D, State, Future] =
    withFallback(defaultName, config)(fallback)

  def withFallback[A, D](name: String, config: HystrixConfig)
                        (fallback: => D)
                        (implicit typeOut: TypeTagTree[Future[D]]): PartialLift[A, D, State, Future] =
    PartialLift(name, State(Some(() => fallback), config), HystrixLiftOps)

  private object HystrixLiftOps extends LiftOps[State, Future] {
    def point[A](given: => A): Future[A] =
      FutureLiftOps.point(given)

    def flatten[A](given: Future[Future[A]])(implicit state: State): Future[A] =
      FutureLiftOps.flatten(given)(state.config.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit state: State): Future[B] =
      FutureLiftOps.map(given)(fn)(state.config.context)

    def liftRunner[A, D](linksChain: LinkChain, opsChain: ChainedLiftOps[Future], runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] = {
      //The fallback must also be lifted up the chain so that it can be applied
      //to the resulting lifted value if necessary.
      lazy val fallbackTo =
        state.fallback
          .map(fn => opsChain.prepoint(fn()))
          .map(_.asInstanceOf[D])

      import state._
      import state.config._
      import rx.functions.Action1

      lazy val setter =
        config.builder(
          HystrixCommand.Setter
            .withGroupKey(HystrixCommandGroupKey.Factory.asKey(config.group))
            .andCommandKey(HystrixCommandKey.Factory.asKey(config.command))
            .andCommandPropertiesDefaults(
              HystrixCommandProperties.Setter()
                .withExecutionIsolationThreadTimeoutInMilliseconds(config.timeout.toMillis.toInt)
            )
        )

      (param: A) => {
        val cmd =
          if (fallbackTo.isDefined) {
            new HystrixCommand[D](setter) {
              override def run() =
                runner(param)

              override def getFallback =
                fallbackTo.get
            }
          } else {
            new HystrixCommand[D](setter) {
              override def run() =
                runner(param)
            }
          }

        //For performance reasons do not capture values in a closure,
        //instead explicitly provide them as arguments.

        class onNext(promise: Promise[D]) extends Action1[D] {
          def call(d: D): Unit =
            promise.success(d)
        }

        class onError(promise: Promise[D]) extends Action1[Throwable] {
          def call(t: Throwable): Unit =
            promise.failure(t)
        }

        val p =
          Promise[D]()

        //No need to convert to a Scala Observable.
        val obs = cmd.observe()
          .first()
          .forEach(new onNext(p), new onError(p))

        p.future
      }

    }
  }
}
