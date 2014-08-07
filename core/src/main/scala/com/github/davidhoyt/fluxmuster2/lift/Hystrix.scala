package com.github.davidhoyt.fluxmuster2.lift

import akka.util.Timeout
import com.netflix.hystrix.HystrixCommand
import rx.functions.{Action0, Action1}
import scala.concurrent.duration._

import com.github.davidhoyt.fluxmuster2._
import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

import scala.util.control.NonFatal

case class HystrixConfiguration(group: String = "default", command: String = "default", timeout: Duration = 1.second, builder: HystrixCommand.Setter => HystrixCommand.Setter = identity)

object Hystrix {
  import com.netflix.hystrix.{HystrixCommandGroupKey, HystrixCommandKey, HystrixCommandProperties}
  import rx.{Subscription, Subscriber}
  import scala.concurrent.ExecutionContext
  import scala.concurrent.{Promise, Future}

  //B = return value or fallback
  //A = optional value to call the hystrix command with
  type HystCommandNeedsFallback[-A, B] = (() => B) => A => Future[B]
  type HystCommand[-A, +B] = A => Future[B]

  val NAME = Macros.simpleNameOf[Hystrix.type]

  case class State[T](fallback: () => T, configuration: HystrixConfiguration, lifted: Boolean)(implicit val timeout: Timeout, val context: ExecutionContext, val typeFallback: TypeTagTree[T], val typeLiftedFallback: TypeTagTree[Future[T]])

  def apply[T](name: String)
              (fallback: => T = throw new UnsupportedOperationException("No fallback available"))
              (implicit configuration: HystrixConfiguration, timeout: Timeout, executionContext: ExecutionContext, typeFallback: TypeTagTree[T], typeLiftedFallback: TypeTagTree[Future[T]], typeState: TypeTagTree[State[T]]): LiftedNeedsStep[State[T], Future] =
    apply(name, configuration)(fallback)

  def apply[T](name: String, configuration: HystrixConfiguration)
              (fallback: => T = throw new UnsupportedOperationException("No fallback available"))
              (implicit timeout: Timeout, executionContext: ExecutionContext, typeFallback: TypeTagTree[T], typeLiftedFallback: TypeTagTree[Future[T]], typeState: TypeTagTree[State[T]]): LiftedNeedsStep[State[T], Future] = {

    require(timeout.duration.isFinite(), s"Hystrix timeout must be a finite amount")

    //Override the configuration with the provided timeout.
    val adjustedConfiguration = configuration.copy(timeout = timeout.duration)
    val state = State(() => fallback, adjustedConfiguration, false)

    LiftedNeedsStep(name, state, new HystrixOps[T], mapStateOnLiftAndFlatten = {(s: State[T]) =>
      s.copy(lifted = true)})
  }

  class HystrixOps[T] extends LiftOp[State[T], Future] {
    implicit def point[A](given: => A)(implicit state: State[T]): Future[A] =
      FutureLiftOp.point(given)(state.context)

    implicit def flatten[A](given: Future[Future[A]])(implicit state: State[T]): Future[A] =
      FutureLiftOp.flatten(given)(state.context)

    implicit def map[A, B](given: Future[A])(fn: A => B)(implicit state: State[T]): Future[B] =
      FutureLiftOp.map(given)(fn)(state.context)

    implicit def apply[A, D](runner: A => D)(implicit state: State[T], connections: Connections, shouldLiftResult: Boolean, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => Future[D] = {
      //typeResult = Future[D]
      //state.typeLiftedFallback = Future[T]
      val unliftedTypeArgumentResult = typeResult.typeArguments.head.tpe
      val unliftedTypeArgumentFallback = state.typeLiftedFallback.typeArguments.head.tpe
      val convertedState = state.asInstanceOf[State[D]]
      val convertedOps = this.asInstanceOf[HystrixOps[D]]

      require(unliftedTypeArgumentResult <:< unliftedTypeArgumentFallback, s"$unliftedTypeArgumentResult is not a subtype of $unliftedTypeArgumentFallback")
      construct[A, D](convertedState, convertedOps)(runner).apply(convertedState.fallback)
    }
  }



  private def construct[A, D](state: State[D], ops: HystrixOps[D])(runner: A => D): HystCommandNeedsFallback[A, D] =
    (providedFallback: () => D) => {
      import scala.util._

      sealed trait HystrixResult[D] {
        val result: D
      }
      sealed case class HystrixFallback[D](result: D) extends HystrixResult[D]
      sealed case class HystrixSuccess[D](result: D) extends HystrixResult[D]

      //Place this outside the returned function in order to ensure
      //it's evaluated only once across multiple invocations of the returned
      //function.
      lazy val fallbackTo =
        HystrixFallback(providedFallback())
        //try {
        //  Future.successful(providedFallback())
        //} catch {
        //  case NonFatal(error) =>
        //    Future.failed(error)
        //}

      import state._

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
          val cmd = new HystrixCommand[HystrixResult[D]](setter) {
          override def run() = {
            //require(false)
            HystrixSuccess(runner(param))
          }

          override def getFallback =
            fallbackTo
        }

        val p = Promise[D]()

        import rx.lang.scala.JavaConversions.toScalaObservable
        val o = toScalaObservable(cmd.observe())
          .first
          //This is an odd workaround. Hystrix is providing 2 different
          //shapes depending on if the result uses the fallback or not.
          //We get around this by wrapping results into a known type and
          //then adjusting the shape based on those types.
          .map(_ match {
            case HystrixFallback(result) =>
              Future.successful(result).asInstanceOf[D]
            case HystrixSuccess(result) =>
              result.asInstanceOf[D]
          })
        val subscription = o.subscribe(p.success(_), p.failure(_))
        val f = p.future
        f.onComplete(_ => subscription.unsubscribe())
        f
      }
    }
}
