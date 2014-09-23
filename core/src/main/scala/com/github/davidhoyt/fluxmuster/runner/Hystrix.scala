package com.github.davidhoyt.fluxmuster.runner

import com.github.davidhoyt.fluxmuster._
import com.netflix.hystrix.HystrixCommand
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

case class HystrixConfiguration(group: String = "default", command: String = "default", implicit val timeout: Duration = 1.second, builder: HystrixCommand.Setter => HystrixCommand.Setter = identity)(implicit val context: ExecutionContext)

object Hystrix {
  import com.netflix.hystrix.{HystrixCommandGroupKey, HystrixCommandKey, HystrixCommandProperties}
  import scala.concurrent.{Promise, Future}
  import Chains._

  //B = return value or fallback
  //A = optional value to call the hystrix command with
  type HystCommandNeedsFallback[-A, B] = Option[() => Any] => A => Future[B]
  type HystCommand[-A, +B] = A => Future[B]

  val defaultName = Macros.simpleNameOf[Hystrix.type]

  case class State private[Hystrix] (fallback: Option[() => Any], configuration: HystrixConfiguration)
                                    (implicit val typeFallback: TypeTagTree[Any], val typeLiftedFallback: TypeTagTree[Future[Any]])

  def apply[A, D](name: String = defaultName, configuration: HystrixConfiguration)
                 (implicit typeOut: TypeTagTree[Future[D]]): RunnerNeedsProxy[A, D, State, Future] =
    RunnerNeedsProxy(name, State(None, configuration), HystrixRunnerOps, mapStateOnLift)

  def withFallback[A, D](name: String = defaultName, configuration: HystrixConfiguration)
                        (fallback: => D)
                        (implicit typeOut: TypeTagTree[Future[D]]): RunnerNeedsProxy[A, D, State, Future] =
    RunnerNeedsProxy(name, State(Some(() => fallback), configuration), HystrixRunnerOps, mapStateOnLift)

  private object HystrixRunnerOps extends RunnerOps[State, Future] {
    def point[A](given: => A): Future[A] =
      FutureRunnerOps.point(given)

    def flatten[A](given: Future[Future[A]])(implicit state: State): Future[A] =
      FutureRunnerOps.flatten(given)(state.configuration.context)

    def map[A, B](given: Future[A])(fn: A => B)(implicit state: State): Future[B] =
      FutureRunnerOps.map(given)(fn)(state.configuration.context)

    def liftRunner[A, D](linksChain: LinkChain, opsChain: ChainedRunnerOps[Future], runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] = {


//      //typeOut = Future[D]
//      //state.typeLiftedFallback = Future[T]
//      val unliftedTypeArgumentOut = typeOut.tpe
//      val unliftedTypeArgumentFallback = state.typeLiftedFallback.typeArguments.head.tpe
//      val convertedState = state.asInstanceOf[State[D]]
//      val convertedOps = this.asInstanceOf[HystrixOps[D]]
//
//      require(unliftedTypeArgumentFallback <:< unliftedTypeArgumentOut, s"$unliftedTypeArgumentOut is not a subtype of $unliftedTypeArgumentFallback")
//      construct[A, D](convertedState, convertedOps)(runner).apply(convertedState.fallback)
      //construct[A, D](state)(runner).apply(state.fallback)

      val lifted = FutureRunnerOps.liftRunner(linksChain, opsChain, runner)(scala.concurrent.ExecutionContext.Implicits.global, typeIn, typeOut)
      lazy val fallback = opsChain.point(state.fallback.get.apply().asInstanceOf[D])
      (a: A) => {
        lifted(a) fallbackTo {
          fallback
        }
      }

    }
  }

  def mapStateOnLift(state: State, other: RunnerDataChain): State = {
    //val f = liftChainRunnerPoint(other, state)
//    val foo = state.fallback.map(f => liftChainRunnerPoint(other, f()): Any)
//    val liftedFallback = state.fallback.map(f => () => liftChainRunnerPoint(other, f()): Any)
    //The fallback must also be lifted up the chain so that it can be applied
    //to the resulting lifted value if necessary.
//        val liftedFallback = other.foldLeft(state.fallback) {
//          case (fall, part) =>
//            fall map (f => () => )
//        }

    //Create a new state where the fallback has been properly lifted into
    //context and which should be used for runs.
//    val newState = state.copy(fallback = liftedFallback)
//    newState
    ???
  }

  private def create[A, D](providedName: String, configuration: HystrixConfiguration, chained: Chained[A, D], fallback: => Option[() => D])(implicit typeOut: TypeTagTree[Future[D]]): RunnerNeedsProxy[A, D, State, Future] = {

    import scala.language.higherKinds

    require(configuration.timeout.isFinite(), s"Hystrix timeout must be a finite amount")

//    def mapStateOnLift(state: State, other: ChainRunner): State = {
//      //The fallback must also be lifted up the chain so that it can be applied
//      //to the resulting lifted value if necessary.
//      val liftedFallback = other.foldLeft(state.fallback) {
//        case (fall, part) =>
//          fall map (f => () => part.ops.point(f())(part.ops.unsafeCastAsState(part.state)))
//      }
//
//      //Create a new state where the fallback has been properly lifted into
//      //context and which should be used for runs.
//      val newState = state.copy(fallback = liftedFallback)
//      newState
//    }

    ???
  }
}
