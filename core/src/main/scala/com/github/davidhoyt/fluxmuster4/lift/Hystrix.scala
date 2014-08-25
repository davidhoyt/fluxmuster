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
  type HystCommandNeedsFallback[-A, B] = Option[() => Any] => A => Future[B]
  type HystCommand[-A, +B] = A => Future[B]

  val NAME = Macros.simpleNameOf[Hystrix.type]

  val typeState =
    typeTagTreeOf[State]



  case class State(fallback: Option[() => Any], configuration: HystrixConfiguration)(implicit val typeFallback: TypeTagTree[Any], val typeLiftedFallback: TypeTagTree[Future[Any]])



  def apply(configuration: HystrixConfiguration): LiftNeedsChained[_, State, Future] =
    create[Nothing](NAME, configuration, None)

  def apply(name: String, configuration: HystrixConfiguration): LiftNeedsChained[_, State, Future] =
    create[Nothing](name, configuration, None)

  def withFallback[T](configuration: HystrixConfiguration)
                     (fallback: => T): LiftNeedsChained[T, State, Future] =
    create(NAME, configuration, Some(() => fallback))

  def withFallback[T](name: String, configuration: HystrixConfiguration)
                     (fallback: => T): LiftNeedsChained[T, State, Future] =
    create(name, configuration, Some(() => fallback))



  private def create[T](providedName: String, configuration: HystrixConfiguration, fallback: => Option[() => T]): LiftNeedsChained[T, State, Future] = {

    import scala.collection.immutable

    import scala.language.higherKinds

    require(configuration.timeout.isFinite(), s"Hystrix timeout must be a finite amount")

    val providedTypeState = typeState

    new LiftNeedsChained[T, State, Future] with Named {
      val ops       = HystrixOps
      val state     = State(fallback, configuration)
      val typeState = providedTypeState
      val name      = providedName


      override def lift[A, S, F[_]](other: Lift[A, T, S, F])(implicit converter: F -> Future, typeOut: TypeTagTree[Future[T]], typeFofOut: TypeTagTree[F[T]], typeIntoFofOut: TypeTagTree[Future[F[T]]]): Lift[A, T, State, Future] = {
        //TODO: Lift hystrix fallback ...


        val liftedFallback = other.liftChain.foldLeft(state.fallback) {
          case (fall, part) =>
            fall map (f => () => part.ops.point(f())(part.state))
        }

        val newState = State(liftedFallback, configuration)

        val liftedChain = immutable.Vector(other)
        val liftedLink = runInThisContext(other, other.run)(converter, other.typeIn, other.typeOut, typeOut)



        val f = Lift.create(name, liftedChain, liftedLink, other.liftChain, newState, HystrixOps)(typeState, other.typeIn, typeOut)
        f
      }
    }
  }

  private object HystrixOps extends LiftOps[State, Future] {
    def point[A](given: => A)(implicit state: State): Future[A] =
      FutureLiftOps.point(given)(state.configuration.context)

    def flatten[A](given: Future[Future[A]])(implicit state: State): Future[A] =
      FutureLiftOps.flatten(given)(state.configuration.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit state: State): Future[B] =
      FutureLiftOps.map(given)(fn)(state.configuration.context)

    def liftRunner[A, D](chain: ChainLink, runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] = {
//      //typeOut = Future[D]
//      //state.typeLiftedFallback = Future[T]
//      val unliftedTypeArgumentOut = typeOut.tpe
//      val unliftedTypeArgumentFallback = state.typeLiftedFallback.typeArguments.head.tpe
//      val convertedState = state.asInstanceOf[State[D]]
//      val convertedOps = this.asInstanceOf[HystrixOps[D]]
//
//      require(unliftedTypeArgumentFallback <:< unliftedTypeArgumentOut, s"$unliftedTypeArgumentOut is not a subtype of $unliftedTypeArgumentFallback")
//      construct[A, D](convertedState, convertedOps)(runner).apply(convertedState.fallback)
      construct[A, D](state)(runner).apply(state.fallback)
    }
  }

  private def construct[A, D](state: State)(runner: A => D): HystCommandNeedsFallback[A, D] =
    (providedFallback: Option[() => Any]) => {
      //Place this outside the returned function in order to ensure
      //it's evaluated only once across multiple invocations of the returned
      //function.
      lazy val fallbackTo =
        providedFallback.getOrElse(throw new UnsupportedOperationException("No fallback available")).apply().asInstanceOf[D]

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
