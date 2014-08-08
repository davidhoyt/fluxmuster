package com.github.davidhoyt.fluxmuster2

import akka.actor.ActorSystem
import akka.util.Timeout
import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

import scala.annotation.implicitNotFound
import scala.collection._

trait Runner[A, B, C, D] { this: StepConnections =>
  val downstream: LinkDownstream[A, B]
  val upstream: LinkUpstream[C, D]

  def apply(a: A)(implicit evidence: B <:< C): D =
    run(a)

  def run(a: A)(implicit evidence: B <:< C): D = {
    //println(s"Connections: $connections")
    upstream(evidence(downstream(a)))
  }
}

trait StepConnections {
  val connections: Connections
}

trait StepLike[A, B, C, D] {
  implicit val typeAcceptDownstream: TypeTagTree[A]
  implicit val typeMappedDownstream: TypeTagTree[B]
  implicit val typeAcceptUpstream: TypeTagTree[C]
  implicit val typeMappedUpstream: TypeTagTree[D]

  val name: String

  def asShortString: String =
    null

  lazy val asDefaultString =
    s"$name[${typeAcceptDownstream.toShortString}, ${typeMappedDownstream.toShortString}, ${typeAcceptUpstream.toShortString}, ${typeMappedUpstream.toShortString}]"

  lazy val toShortString = {
    val short = asShortString
    if (short ne null)
      short
    else
      asDefaultString
  }

  override def toString =
    toShortString
}

trait Step[A, B, C, D] extends StepLike[A, B, C, D] with Runner[A, B, C, D] with StepConnections {
  def combine[T, U](step: SimpleStep[B, T, U, C]): Step[A, T, U, D] = {
    val typeAcceptDownstream = Step.this.typeAcceptDownstream
    val typeMappedDownstream = step.typeMappedDownstream
    val typeAcceptUpstream = step.typeAcceptUpstream
    val typeMappedUpstream = Step.this.typeMappedUpstream

    val down = downstream andThen step.downstream
    val up = upstream compose step.upstream

    val connections: Connections =
      (Step.this.connections ++ step.connections).foldLeft(EmptyConnections) {
        case (seq, p) if p.connections.nonEmpty =>
          seq :+ p.connections.head
        case (seq, _) =>
          seq
      }

    Step("<~>", down, up, connections)(typeAcceptDownstream, typeMappedDownstream, typeAcceptUpstream, typeMappedUpstream)
  }

  def lift[S, F[_]](needsStep: LiftedNeedsStep[S, F])(implicit evidence: B <:< C, converter: F -> F, tFofB: TypeTagTree[F[B]], tFofD: TypeTagTree[F[D]]): LiftedAndFlatten[A, D, S, F] =
    needsStep.lift(this)

//  //def combine[T, U, F[_]](lifted: Lifted[T, A, D, U, F]): Step[T, B, C, U] =
//  //  lifted.step.combine(this)
//  def combine[T, U, S, F[_]](lifted: Lifted[B, T, U, C, S, F]): Step[A, T, U, D] =
//    combine(lifted.step)
}

object Step {
  private case class Builder[A, B, C, D](name: String, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], providedConnections: Connections, override val asShortString: String = null)(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedDownstream: TypeTagTree[B], val typeAcceptUpstream: TypeTagTree[C], val typeMappedUpstream: TypeTagTree[D]) extends Step[A, B, C, D] {
    lazy val connections = {
      if ((providedConnections eq null) || providedConnections.isEmpty)
        immutable.Seq(this)
      else
        providedConnections
    }
  }

  def apply[A, B, C, D](name: String, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D] =
    Builder(name, downstream, upstream, null)

  def apply[A, B, C, D](name: String, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], connections: Connections)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D] =
    Builder(name, downstream, upstream, connections)

  def unapply[A, B, C, D](step: SimpleStep[A, B, C, D]): Option[(LinkDownstream[A, B], LinkUpstream[C, D], Connections)] =
    PartialFunction.condOpt(step) {
      case _ => (step.downstream, step.upstream, step.connections)
    }
}

object DownAndUp {
  def apply[A, B, C, D](downstream: LinkDownstream[A, B], upstream: LinkDownstream[C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D] =
    Step("DownAndUp", downstream, upstream)
}

object Identity {
  def apply[A, B](implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): Step[A, A, B, B] =
    Step("Identity", identity[A], identity[B])(tA, tA, tB, tB)
}

object Passthrough {
  def apply[A, B, C, D](step: Step[A, B, C, D]): Step[A, B, C, D] =
    Step("Passthrough", step.downstream, step.upstream)(step.typeAcceptDownstream, step.typeMappedDownstream, step.typeAcceptUpstream, step.typeMappedUpstream)
}

import scala.language.higherKinds
import scala.language.implicitConversions

trait LiftOp[S, F[_]] {
  implicit def apply[A, D](runner: A => D)(implicit state: S, connections: Connections, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => F[D]
  implicit def point[A](given: => A)(implicit state: S): F[A]
  implicit def flatten[A](given: F[F[A]])(implicit state: S): F[A]
  implicit def map[A, B](given: F[A])(fn: A => B)(implicit state: S): F[B]
}

trait LiftOpWithoutState[F[_]] extends LiftOp[Nothing, F] {
  def apply[A, D](runner: A => D): A => F[D]
  def flatten[A](given: F[F[A]]): F[A]
  def map[A, B](given: F[A])(fn: A => B): F[B]
//
//  //Throws away the state.
//
//  def apply[A, D](runner: A => D)(implicit state: Nothing, connections: Connections): A => F[D] =
//    apply(runner)
//
//  def flatten[A](given: F[F[A]])(implicit state: Nothing): F[A] =
//    flatten(given)
//
//  def map[A, B](given: F[A])(fn: A => B)(implicit state: Nothing): F[B] =
//    map(given)(fn)
}

trait LiftLike[S, F[_]] {
  implicit val typeState: TypeTagTree[S]
  implicit val state: S
  implicit val op: LiftOp[S, F]

  protected def runInThisContext[A, D, G[_]](connections: Connections)(runner: A => G[D])(implicit converter: G -> F, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[G[D]]): LinkDownstream[A, F[D]] =
    (a: A) => {
      val runOtherInThisContext: A => F[G[D]] = op.apply(runner)(state, connections, typeAccept, typeResult)
      val resultAfterRunning: F[G[D]] = runOtherInThisContext(a)

      //flatMap!
      val mapResultBackIntoThisContext = op.map(resultAfterRunning)(converter.apply)(state)
      val flattenedBackIntoThisContext: F[D] = op.flatten(mapResultBackIntoThisContext)(state)
      flattenedBackIntoThisContext
    }
}

sealed trait LiftedAndFlatten[A, D, S, F[_]] extends LiftLike[S, F] with StepLike[A, F[D], F[D], F[D]] with Runner[A, F[D], F[D], F[D]] with StepConnections {
  implicit lazy val typeMappedDownstream = typeMappedUpstream
  implicit lazy val typeAcceptUpstream = typeMappedUpstream

//  def lift[U, V, W, G[_]](other: Lifted[A, U, V, G[D], W, G])(implicit e1: U <:< V, converter: G -> F, typeResult: TypeTagTree[G[D]]): LiftedAndFlatten[A, D, S, F] = {
//    val connections = immutable.Seq(other)
//    val f = op.apply(other.run)(state, connections, typeAcceptDownstream, typeAcceptUpstream)
//    LiftedAndFlatten[A, D, S, F](name, runInThisContext(connections)(other.run), state, op, connections)(typeAcceptDownstream, typeMappedUpstream, typeState)
//  }
  def lift[W, G[_]](other: LiftedNeedsStep[W, G])(implicit converter: F -> G, tA: TypeTagTree[A], tFofD: TypeTagTree[F[D]], tGofD: TypeTagTree[G[D]]) =
    other.lift(LiftedAndFlatten.this)(converter, tGofD, tA, tFofD)

  def lift[U, V, W, G[_]](other: LiftedAndFlatten[A, D, W, G])(implicit converter: G -> F, typeResult: TypeTagTree[G[D]]): LiftedAndFlatten[A, D, S, F] = {
    val connections = immutable.Seq(other)
    LiftedAndFlatten[A, D, S, F](name, runInThisContext(connections)(other.run), state, op, connections)(typeAcceptDownstream, typeMappedUpstream, typeState)
  }
}

object LiftedAndFlatten {
  private case class Builder[A, D, S, F[_]](name: String, downstream: LinkDownstream[A, F[D]], upstream: LinkUpstream[F[D], F[D]], state: S, op: LiftOp[S, F], connections: Connections)(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedUpstream: TypeTagTree[F[D]], val typeState: TypeTagTree[S]) extends LiftedAndFlatten[A, D, S, F]

  def apply[A, D, S, F[_]](name: String, downstream: LinkDownstream[A, F[D]], state: S, op: LiftOp[S, F], connections: Connections)(implicit typeAcceptDownstream: TypeTagTree[A], typeMappedUpstream: TypeTagTree[F[D]], typeState: TypeTagTree[S]): LiftedAndFlatten[A, D, S, F] =
    Builder[A, D, S, F](name, downstream, identity[F[D]], state, op, connections)
}

//trait Lifted[A, B, C, D, S, F[_]] extends LiftLike[S, F] with StepLike[A, B, C, F[D]] with Runner[A, B, C, F[D]] with StepConnections {
//  def lift[T, U](other: Step[A, T, U, D])(implicit tFofT: TypeTagTree[F[T]], tFofD: TypeTagTree[F[D]]): Lifted[A, T, U, D, S, F] =
//    Lifted(name, other, state, op)
//
//  def lift[U, V, W, G[_]](other: Lifted[A, U, V, D, W, G])(implicit e1: U <:< V, tFofD: TypeTagTree[F[D]], typeResult: TypeTagTree[D]): LiftedAndFlatten[A, D, S, F] = {
////    val appliedLiftOp = new LiftOp[S, F] {
////      def apply[A1, D1](state: S)(runner: (A1) => D1): (A1) => F[D1] = {
////        val otherToRunInThisInstance: A1 => G[D1] = other.op.apply(other.state)(runner)
////        val converted: A1 => F[D1] = (a: A1) => {
////          val z = otherToRunInThisInstance(a).asInstanceOf[F[D1]]
////          z
////        }
////        val runnerWithThisInstance: A1 => F[F[D1]] = op.apply[A1, F[D1]](state)(converted)
////        val result = runnerWithThisInstance andThen op.flatten(state)
////        result
////      }
////
////      def flatten[A1](state: S)(given: F[F[A1]]): F[A1] =
////        op.flatten(state)(given)
////    }
//
//    val down: LinkDownstream[A, F[D]] =
//      runInThisContext(connections)(other.run)
//      //op.apply(other.run)(state, connections, typeAcceptDownstream, typeResult)
//
//    val up: LinkUpstream[F[D], F[D]] =
//      identity
//
//    LiftedAndFlatten[A, D, S, F](name, down, state, op, immutable.Seq(other))
//  }
//}

//Note: do not refactor to provide state at execution time because it's very possible
//      that different parts of the pipeline will require different states and the # of
//      needed states would depend on what's in the pipeline. So either we avoid it
//      altogether by capturing the state at definition time or we create a registry
//      that can be used as a lookup by the members of the pipeline. We could indeed do
//      both by having the registry be the state!

//object Lifted {
//  private case class Builder[A, B, C, D, S, F[_]](name: String, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], state: S, op: LiftOp[S, F], connections: Connections = immutable.Seq())(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedDownstream: TypeTagTree[B], val typeAcceptUpstream: TypeTagTree[C], val typeMappedUpstream: TypeTagTree[D], val typeState: TypeTagTree[S]) extends Lifted[A, B, C, D, S, F]
//
//  def apply[A, B, C, D, S, F[_]](name: String, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], state: S, op: LiftOp[S, F], connections: Connections = immutable.Seq())(implicit typeAcceptDownstream: TypeTagTree[A], typeMappedDownstream: TypeTagTree[B], typeAcceptUpstream: TypeTagTree[C], typeMappedUpstream: TypeTagTree[D], typeState: TypeTagTree[S]): Lifted[A, B, C, D, S, F] =
//    Builder(name, downstream, upstream, state, op)
//
//  def apply[A, B, C, D, S, F[_]](name: String, step: SimpleStep[A, B, C, D], state: S, op: LiftOp[S, F])(implicit typeState: TypeTagTree[S]): Lifted[A, B, C, D, S, F] =
//    Builder[A, B, C, D, S, F](name, step.downstream, step.upstream, state, op, immutable.Seq(step))(step.typeAcceptDownstream, step.typeMappedDownstream, step.typeAcceptUpstream, step.typeMappedUpstream, typeState)
//
////  def apply[A, B, C, D, F[_]](step: Step[A, B, C, D])(liftWith: (A => D) => (A => F[D]))(implicit typeState: TypeTagTree[Nothing], typeLiftIntoBridge: TypeTagTree[F[B]], typeLiftIntoUpstream: TypeTagTree[F[D]]): Lifted[A, B, C, D, Nothing, F] =
////    Builder[A, B, C, D, Nothing, F](step, new StatelessLiftOp[F] {
////      override def apply[T, U](runner: T => D): (A) => F[D] = liftWith(runner)
////    })
//}

import scala.concurrent._
object Implicits {
  implicit val futureLiftOp = FutureLiftOp
}

object LiftUpstream {
  def apply[A, D, F[_]](lift: D => F[D])(implicit tA: TypeTagTree[A], tD: TypeTagTree[D], tFofD: TypeTagTree[F[D]]): Step[A, A, D, F[D]] =
    Step("LiftUpstream", identity[A], identity[D]_ andThen lift)
}

trait LiftedNeedsStep[S, F[_]] extends LiftLike[S, F] /*with StepConnections*/ {
  val name: String

  def lift[A, B, C, D](step: SimpleStep[A, B, C, D])(implicit e1: B <:< C, converter: F -> F, typeLiftIntoBridge: TypeTagTree[F[B]], typeLiftIntoUpstream: TypeTagTree[F[D]]): LiftedAndFlatten[A, D, S, F] = {
    import step.typeAcceptDownstream
    val liftedUpstream = LiftUpstream[A, D, F]((d: D) => op.point(d))(step.typeAcceptDownstream, step.typeMappedUpstream, typeLiftIntoUpstream)
    val combinedStep = liftedUpstream combine step
    val connections = liftedUpstream +: step.connections
    LiftedAndFlatten[A, D, S, F](name, runInThisContext[A, D, F](connections)(combinedStep.run), state, op, connections)(step.typeAcceptDownstream, typeLiftIntoUpstream, typeState)
  }

  //S = Akka.State
  //F = Future
  //W = Hystrix.State
  //G = Future
//  def lift[W, G[_]](other: LiftedNeedsStep[W, G])(implicit converter: G -> F): LiftedNeedsStep[W, G] = {
//    val connections = EmptyConnections
//    val appliedLiftOp = new LiftOp[W, G] {
//      def point[A](given: => A)(implicit state: S): G[A]
//      def flatten[A](given: G[G[A]])(implicit state: S): G[A]
//      def map[A, B](given: G[A])(fn: A => B)(implicit state: S): G[B]
//      def apply[A, D](runner: A => D)(implicit state: S, connections: Connections, shouldLiftResult: Boolean, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => G[D] = {
//        op.apply(other)
//      }
//
//      def apply[A1, D1](state: S)(runner: (A1) => D1): (A1) => F[D1] = {
//        val otherToRunInThisInstance: A1 => G[D1] = other.op.apply(other.state)(runner)
//        val converted: A1 => F[D1] = (a: A1) => {
//          val z = otherToRunInThisInstance(a).asInstanceOf[F[D1]]
//          z
//        }
//        val runnerWithThisInstance: A1 => F[F[D1]] = op.apply[A1, F[D1]](state)(converted)
//        val result = runnerWithThisInstance andThen op.flatten(state)
//        result
//      }
//
//      def flatten[A1](state: S)(given: F[F[A1]]): F[A1] =
//        op.flatten(state)(given)
//    }
//    LiftedNeedsStep(other.name, other.state, )
//  }

  def lift[A, D, W, G[_]](other: LiftedAndFlatten[A, D, W, G])(implicit converter: G -> F, typeLiftIntoUpstream: TypeTagTree[F[D]], typeAccept: TypeTagTree[A], typeResult: TypeTagTree[G[D]]): LiftedAndFlatten[A, D, S, F] = {
    val connections = immutable.Seq(other)
    LiftedAndFlatten[A, D, S, F](name, runInThisContext(connections)(other.run), state, op, connections)(other.typeAcceptDownstream, typeLiftIntoUpstream, typeState)
  }
}

object LiftedNeedsStep {
  private case class Builder[S, F[_]](name: String, state: S, op: LiftOp[S, F])(implicit val typeState: TypeTagTree[S]) extends LiftedNeedsStep[S, F]

  def apply[S, F[_]](name: String, state: S, op: LiftOp[S, F])(implicit typeState: TypeTagTree[S]): LiftedNeedsStep[S, F] =
    Builder(name, state, op)
}

object RunInFuture {
  val NAME = Macros.simpleNameOf[RunInFuture.type]

  def apply(name: String = NAME)(implicit ec: ExecutionContext, liftOp: LiftOp[ExecutionContext, Future]): LiftedNeedsStep[ExecutionContext, Future] =
    LiftedNeedsStep(name, ec, liftOp)
}

object Test extends App {
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  //import Implicits._

  implicit val timeout = Timeout(10.seconds)
  implicit val system = ActorSystem("test")

  val IntIdentity = Identity[Int, Int]
  val LongIdentity = Identity[Long, Long]
  val IntLongIdentity = Identity[Int, Long]
  val IntStringIdentity = Identity[Int, String]
  val IntLongMap = DownAndUp((x: Int) => x, (x: Int) => x.toLong)
  val IntLongStringMap = DownAndUp((x: Int) => x, (x: Long) => x.toString)

  val IntPrintIdentity = DownAndUp({x: Int => println(s"[${Thread.currentThread().getName}] Downstream: $x"); x}, {x: Int => println(s"[${Thread.currentThread().getName}] Upstream: $x"); x})
  val IntStringPrintIdentity = DownAndUp({x: Int => println(s"[${Thread.currentThread().getName}] Downstream: $x"); x.toString}, {x: String => println(s"[${Thread.currentThread().getName}] Upstream: $x"); x})

  //val LiftedIntIdentity = RunInFuture() lift IntIdentity
//  val LiftedIntStringIdentity = RunInFuture() lift IntStringIdentity
//  val LiftedIntLongStringMap = RunInFuture() lift IntLongStringMap
  //val LiftedIntStringPrintIdentity = RunInFuture() lift IntStringPrintIdentity
//  val LiftedLifted = LiftedIntStringIdentity lift LiftedIntStringPrintIdentity

  val IncrementBy2 = DownAndUp({x: Int => x + 1}, {x: Int => x + 1})
  val a1 = DownAndUp({x: Int => x.toString}, {x: Int => x.toLong + 2})
  val a2 = DownAndUp({x: String => x.toInt + 1}, {x: Int => x + 2})

  val a3: Step[Int, Int, Long, Long] = Identity[Int, Long]
  val a4: Step[Int, Int, Int, Long] = a1 combine a2
//  val l1: LiftedAndFlatten[Int, Long, ExecutionContext, Future] = RunInFuture() lift a4
  //val l1step: Step[Int, Int, Int, Long] = l1.step
  val foo = a3 combine a4
  //val a5 = a3 combine l1
  //val l2 = l1 combine IntPrintIdentity
  //val l2 = LiftedFuture(a5)
//  val f1 = LiftedLifted(0)

//  val ff1: LiftedAndFlatten[Int, Long, ExecutionContext, Future] = RunInFuture() lift (a1 combine a2 combine IntIdentity combine IntIdentity)
////  val ff2: LiftedAndFlatten[Int, Long, ExecutionContext, Future] = RunInFuture() lift IntLongIdentity
////  val ff3 = ff2 lift ff1
////  val ff4 = ff3 lift ff3
//  val ff5 = lift.Akka.par(lift.AkkaConfiguration()) lift ff1
//  val ff6 = lift.Hystrix("???", lift.HystrixConfiguration())(myFallback) lift ff5

  val parAkkaWithCombine = a3 combine a1 combine a2 lift lift.Akka.par(lift.AkkaConfiguration()) lift lift.Hystrix("???", lift.HystrixConfiguration())(10L)
  println(Await.result(parAkkaWithCombine(0), 10.seconds))

  val parAkka: LiftedAndFlatten[Int, Int, lift.Akka.State, Future] = lift.Akka.par(lift.AkkaConfiguration()) lift IncrementBy2
  val serialAkka: LiftedAndFlatten[Int, Int, lift.Akka.State, Future] = lift.Akka(lift.AkkaConfiguration()) lift IncrementBy2
  val parAkkaHystrix: LiftedAndFlatten[Int, Int, lift.Akka.State, Future] = lift.Akka.par(lift.AkkaConfiguration()) lift (lift.Hystrix("???", lift.HystrixConfiguration())(20) lift IncrementBy2)
  val serialAkkaHystrix: LiftedAndFlatten[Int, Int, lift.Akka.State, Future] = lift.Akka(lift.AkkaConfiguration()) lift (lift.Hystrix("???", lift.HystrixConfiguration())(20) lift IncrementBy2)
  val parHystrixAkka: LiftedAndFlatten[Int, Int, lift.Hystrix.State[Int], Future] =  lift.Hystrix("???", lift.HystrixConfiguration())(20) lift (lift.Akka.par(lift.AkkaConfiguration()) lift IncrementBy2)
  val serialHystrixAkka: LiftedAndFlatten[Int, Int, lift.Hystrix.State[Int], Future] =  lift.Hystrix("???", lift.HystrixConfiguration())(20) lift (lift.Akka(lift.AkkaConfiguration()) lift IncrementBy2)
  val parHystrixHystrixAkka: LiftedAndFlatten[Int, Int, lift.Hystrix.State[Int], Future] =  lift.Hystrix("???", lift.HystrixConfiguration())(200) lift (lift.Hystrix("???", lift.HystrixConfiguration())(20) lift (lift.Akka.par(lift.AkkaConfiguration()) lift IncrementBy2))
  val serialHystrixHystrixAkka: LiftedAndFlatten[Int, Int, lift.Hystrix.State[Int], Future] =  lift.Hystrix("???", lift.HystrixConfiguration())(200) lift (lift.Hystrix("???", lift.HystrixConfiguration())(20) lift (lift.Akka(lift.AkkaConfiguration()) lift IncrementBy2))
  println(Await.result(parAkka(0), 10.seconds))
  println(Await.result(serialAkka(0), 10.seconds))
  println(Await.result(parAkkaHystrix(0), 10.seconds))
  println(Await.result(serialAkkaHystrix(0), 10.seconds))
  println(Await.result(parHystrixAkka(222), 10.seconds))
  println(Await.result(serialHystrixAkka(111), 10.seconds))
  println(Await.result(parHystrixHystrixAkka(111), 10.seconds))
  println(Await.result(serialHystrixHystrixAkka(111), 10.seconds))

  val qqq = lift.Hystrix("???", lift.HystrixConfiguration())(20L) lift foo
  println(Await.result(qqq(0), 10.seconds))

  def myFallback: Long =
    //if (false)
      20L
    //else
    //  throw new IllegalStateException(":(")



//  val fstream = {
//    for {
//      i <- 0 to 1000
//    } yield {
//      ff6(i) map (_ => ())
//    }
//  }
//
//  Await.result(Future.sequence(fstream), 10.seconds)
  println(s"Done")
  system.shutdown()
  system.awaitTermination()
}
