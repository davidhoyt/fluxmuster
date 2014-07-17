package com.github.davidhoyt.fluxmuster

import scala.concurrent.duration._

case class HystrixConfiguration(group: String, command: String, timeout: Duration = 1.second)

object Hystrix {
  import com.netflix.hystrix.{HystrixCommand, HystrixCommandGroupKey, HystrixCommandKey, HystrixCommandProperties}
  import rx.{Subscription, Subscriber}
  import scala.concurrent.ExecutionContext
  import scala.concurrent.{Promise, Future, future}

  //B = return value or fallback
  //A = optional value to call the hystrix command with
  type HystCommandNeedsFallback[-A, B] = B => A => Future[B]
  type HystCommand[-A, +B] = A => Future[B]

  def apply[A, B, C](fallback: => C = throw new UnsupportedOperationException("No fallback available"))
                    (implicit configuration: HystrixConfiguration, executor: ExecutionContext): ProxyLift[A, B, B, C, A, Future[C], Future[C], Future[C]] = {
    require(configuration.timeout.isFinite(), s"Hystrix timeout must be a finite amount")

    (p2: ProxySpecification[A, B, B, C]) => {
      val downstream = construct[A, C](configuration)(ProxySpecification.run(p2)).apply(fallback)
      val upstream = identity[Future[C]]_
      ProxySpecification(Macros.nameOf[Hystrix.type] +: p2.metadata, downstream, upstream, p2.connections)
    }
  }

  private def construct[A, B](configuration: HystrixConfiguration)(fn: A => B)(implicit executor: ExecutionContext): HystCommandNeedsFallback[A, B] =
    (fallback: B) => (param: A) => {
      lazy val fallbackTo = fallback
      lazy val setter =
        HystrixCommand.Setter
          .withGroupKey(HystrixCommandGroupKey.Factory.asKey(configuration.group))
          .andCommandKey(HystrixCommandKey.Factory.asKey(configuration.command))
          .andCommandPropertiesDefaults(
            HystrixCommandProperties.Setter()
              .withExecutionIsolationThreadTimeoutInMilliseconds(configuration.timeout.toMillis.toInt)
          )

      val cmd = new HystrixCommand[B](setter) {
        override def run() =
          fn(param)

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
