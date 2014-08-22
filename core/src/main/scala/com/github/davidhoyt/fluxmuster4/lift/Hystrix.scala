package com.github.davidhoyt.fluxmuster4.lift

import com.netflix.hystrix.HystrixCommand
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import com.github.davidhoyt.fluxmuster4._
import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

case class HystrixConfiguration(group: String = "default", command: String = "default", implicit val timeout: Duration = 1.second, builder: HystrixCommand.Setter => HystrixCommand.Setter = identity)(implicit val context: ExecutionContext)

object Hystrix {
  import com.netflix.hystrix.{HystrixCommandGroupKey, HystrixCommandKey, HystrixCommandProperties}
  import scala.concurrent.{Promise, Future}

  //B = return value or fallback
  //A = optional value to call the hystrix command with
  type HystCommandNeedsFallback[-A, B] = Option[() => B] => A => Future[B]
  type HystCommand[-A, +B] = A => Future[B]

  val NAME = Macros.simpleNameOf[Hystrix.type]

  val typeStateOfNothing =
    typeTagTreeOf[State[Nothing]]



  case class State[T](fallback: Option[() => T], configuration: HystrixConfiguration)(implicit val typeFallback: TypeTagTree[T], val typeLiftedFallback: TypeTagTree[Future[T]])



  def apply(configuration: HystrixConfiguration): LiftNeedsChained[State[Nothing], Future] =
    create[Nothing](NAME, configuration, None)(typeNothing, typeFutureOfNothing, typeStateOfNothing)

  def apply(name: String, configuration: HystrixConfiguration): LiftNeedsChained[State[Nothing], Future] =
    create[Nothing](name, configuration, None)(typeNothing, typeFutureOfNothing, typeStateOfNothing)

  def withFallback[T](configuration: HystrixConfiguration)
                     (fallback: => T)
                     (implicit typeFallback: TypeTagTree[T], typeLiftedFallback: TypeTagTree[Future[T]], typeState: TypeTagTree[State[T]]): LiftNeedsChained[State[T], Future] =
    create(NAME, configuration, Some(() => fallback))

  def withFallback[T](name: String, configuration: HystrixConfiguration)
                     (fallback: => T)
                     (implicit typeFallback: TypeTagTree[T], typeLiftedFallback: TypeTagTree[Future[T]], typeState: TypeTagTree[State[T]]): LiftNeedsChained[State[T], Future] =
    create(name, configuration, Some(() => fallback))



  private def create[T](name: String, configuration: HystrixConfiguration, fallback: => Option[() => T])
              (implicit typeFallback: TypeTagTree[T], typeLiftedFallback: TypeTagTree[Future[T]], typeState: TypeTagTree[State[T]]): LiftNeedsChained[State[T], Future] = {

    require(configuration.timeout.isFinite(), s"Hystrix timeout must be a finite amount")
    LiftNeedsChained(name, State(fallback, configuration), new HystrixOps[T])
  }

  private class HystrixOps[T] extends LiftOps[State[T], Future] {
    def point[A](given: => A)(implicit state: State[T]): Future[A] =
      FutureLiftOps.point(given)(state.configuration.context)

    def flatten[A](given: Future[Future[A]])(implicit state: State[T]): Future[A] =
      FutureLiftOps.flatten(given)(state.configuration.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit state: State[T]): Future[B] =
      FutureLiftOps.map(given)(fn)(state.configuration.context)

    def liftRunner[A, D](chain: ChainLink, runner: A => D)(implicit state: State[T], typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] = {
      //typeOut = Future[D]
      //state.typeLiftedFallback = Future[T]
      val unliftedTypeArgumentOut = typeOut.tpe
      val unliftedTypeArgumentFallback = state.typeLiftedFallback.typeArguments.head.tpe
      val convertedState = state.asInstanceOf[State[D]]
      val convertedOps = this.asInstanceOf[HystrixOps[D]]

      require(unliftedTypeArgumentFallback <:< unliftedTypeArgumentOut, s"$unliftedTypeArgumentOut is not a subtype of $unliftedTypeArgumentFallback")
      construct[A, D](convertedState, convertedOps)(runner).apply(convertedState.fallback)
    }
  }

  private def construct[A, D](state: State[D], ops: HystrixOps[D])(runner: A => D): HystCommandNeedsFallback[A, D] =
    (providedFallback: Option[() => D]) => {
      //Place this outside the returned function in order to ensure
      //it's evaluated only once across multiple invocations of the returned
      //function.
      lazy val fallbackTo =
        providedFallback.getOrElse(throw new UnsupportedOperationException("No fallback available")).apply()

      import state._
      import state.configuration._

      lazy val setter =
        configuration.builder(
          HystrixCommand.Setter
            .withGroupKey(HystrixCommandGroupKey.Factory.asKey(configuration.group))
            .andCommandKey(HystrixCommandKey.Factory.asKey(configuration.command))
            .andCommandPropertiesDefaults(
              HystrixCommandProperties.Setter()
                .withExecutionIsolationThreadTimeoutInMilliseconds(configuration.timeout.toMillis.toInt)
            )
        )

      (param: A) => {
        val cmd =
          if (providedFallback.isDefined) {
            new HystrixCommand[D](setter) {
              override def run() =
                runner(param)

              override def getFallback =
                fallbackTo
            }
          } else {
            new HystrixCommand[D](setter) {
              override def run() =
                runner(param)
            }
          }

        val p = Promise[D]()

        import rx.lang.scala.JavaConversions.toScalaObservable
        val o = toScalaObservable(cmd.observe()).first
        val subscription = o.subscribe(p.success(_), p.failure(_))
        val f = p.future
        f.onComplete(_ => subscription.unsubscribe())
        f
      }
    }
}
