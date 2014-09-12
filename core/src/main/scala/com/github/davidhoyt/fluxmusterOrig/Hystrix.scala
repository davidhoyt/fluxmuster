package com.github.davidhoyt.fluxmuster

import akka.actor.ActorRefFactory
import akka.util.Timeout
import com.netflix.hystrix.HystrixCommand
import scala.concurrent.duration._

case class HystrixConfiguration(group: String = "default", command: String = "default", timeout: Duration = 1.second, builder: HystrixCommand.Setter => HystrixCommand.Setter = identity)

object Hystrix {
  import com.netflix.hystrix.{HystrixCommandGroupKey, HystrixCommandKey, HystrixCommandProperties}
  import rx.{Subscription, Subscriber}
  import scala.concurrent.ExecutionContext
  import scala.concurrent.{Promise, Future, future}

  //B = return value or fallback
  //A = optional value to call the hystrix command with
  type HystCommandNeedsFallback[-A, B] = B => A => Future[B]
  type HystCommand[-A, +B] = A => Future[B]

  val NAME = Macros.simpleNameOf[Hystrix.type]

//  def apply[T](fallback: => T = throw new UnsupportedOperationException("No fallback available"))
//              (implicit configuration: HystrixConfiguration, timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): ProxyLiftDownstreamWithHint[T, Future] =
//    apply(configuration)(fallback)

  def apply[T](configuration: HystrixConfiguration)(fallback: => T = throw new UnsupportedOperationException("No fallback available"))
              (implicit timeout: Timeout, executionContext: ExecutionContext, actorRefFactory: ActorRefFactory): ProxyLiftDownstreamWithHint[T, Future] = {

    require(timeout.duration.isFinite(), s"Hystrix timeout must be a finite amount")

    //Override the configuration with the provided timeout.
    val adjustedConfiguration = configuration.copy(timeout = timeout.duration)

    new ProxyLiftDownstreamWithHint[T, Future] {
      val name = NAME
      def downstream[A, B, C](step: ProxyStep[A, B, B, C])(implicit convert: T => C, tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): LinkDownstream[A, Future[C]] =
        construct[A, C](adjustedConfiguration)(ProxyStep.run(step)).apply(fallback)
    }
  }

  private def construct[A, B](configuration: HystrixConfiguration)(fn: A => B)(implicit executor: ExecutionContext): HystCommandNeedsFallback[A, B] =
    (fallback: B) => {
      //Place this outside the returned function in order to ensure
      //it's evaluated only once across multiple invocations of the returned
      //function.
      lazy val fallbackTo =
        fallback

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
        val cmd = new HystrixCommand[B](setter) {
          override def run() = {
            println("Running in hystrix")
            fn(param)
          }

          override def getFallback =
            fallback
        }

        val p = Promise[B]()

        //Avoiding conversion to rx.scala.Observable since there's no need to do
        //the implicit conversion.
        val o = cmd.observe()

        val subscription: Subscription = o.subscribe(new Subscriber[B]() {
          override def onNext(result: B): Unit =
            p.success(result)

          override def onError(t: Throwable): Unit =
            p.failure(t)

          override def onCompleted(): Unit =
            ()
        })

        val future = p.future
        future.onComplete(_ => subscription.unsubscribe())
        future
      }
    }
}
