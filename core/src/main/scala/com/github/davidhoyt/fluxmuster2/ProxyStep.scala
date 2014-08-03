package com.github.davidhoyt.fluxmuster2

import com.github.davidhoyt.fluxmuster.TypeTagTree

import scala.annotation.implicitNotFound
import scala.collection._

trait Runner[A, B, C, D] {
  val downstream: LinkDownstream[A, B]
  val upstream: LinkUpstream[C, D]

  def apply(a: A)(implicit evidence: B <:< C): D =
    run(a)

  def run(a: A)(implicit evidence: B <:< C): D =
    upstream(evidence(downstream(a)))
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

trait Step[A, B, C, D] extends StepLike[A, B, C, D] with Runner[A, B, C, D] {
  val connections: immutable.Seq[Step[_, _, _, _]]

  def combine[T, U](step: Step[B, T, U, C]): Step[A, T, U, D] = {
    val typeAcceptDownstream = Step.this.typeAcceptDownstream
    val typeMappedDownstream = step.typeMappedDownstream
    val typeAcceptUpstream = step.typeAcceptUpstream
    val typeMappedUpstream = Step.this.typeMappedUpstream

    val down = downstream andThen step.downstream
    val up = upstream compose step.upstream

    val connections: immutable.Seq[Step[_, _, _, _]] =
      (Step.this.connections ++ step.connections).foldLeft(immutable.Seq[Step[_, _, _, _]]()) {
        case (seq, p) if p.connections.nonEmpty =>
          seq :+ p.connections.head
        case (seq, _) =>
          seq
      }

    Step("<~>", down, up, connections)(typeAcceptDownstream, typeMappedDownstream, typeAcceptUpstream, typeMappedUpstream)
  }

//  //def combine[T, U, F[_]](lifted: Lifted[T, A, D, U, F]): Step[T, B, C, U] =
//  //  lifted.step.combine(this)
//  def combine[T, U, S, F[_]](lifted: Lifted[B, T, U, C, S, F]): Step[A, T, U, D] =
//    combine(lifted.step)
}

object Step {
  private case class Builder[A, B, C, D](name: String, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], connections: immutable.Seq[Step[_, _, _, _]], override val asShortString: String = null)(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedDownstream: TypeTagTree[B], val typeAcceptUpstream: TypeTagTree[C], val typeMappedUpstream: TypeTagTree[D]) extends Step[A, B, C, D]

  def apply[A, B, C, D](name: String, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], connections: immutable.Seq[Step[_, _, _, _]] = immutable.Seq())(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D] =
    Builder(name, downstream, upstream, connections)

  def unapply[A, B, C, D](step: Step[A, B, C, D]): Option[(LinkDownstream[A, B], LinkUpstream[C, D], immutable.Seq[Step[_, _, _, _]])] =
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
  implicit def apply[A, D](runner: A => D)(implicit state: S): A => F[D]
  implicit def flatten[A](given: F[F[A]])(implicit state: S): F[A]
  implicit def map[A, B](given: F[A])(fn: A => B)(implicit state: S): F[B]
}

trait LiftOpWithoutState[F[_]] extends LiftOp[Nothing, F] {
  def apply[A, D](runner: A => D): A => F[D]
  def flatten[A](given: F[F[A]]): F[A]
  def map[A, B](given: F[A])(fn: A => B): F[B]

  //Throws away the state.

  def apply[A, D](state: Nothing)(runner: A => D): A => F[D] =
    apply(runner)

  def flatten[A](state: Nothing)(given: F[F[A]]): F[A] =
    flatten(given)

  def map[A, B](state: Nothing)(given: F[A])(fn: A => B): F[B] =
    map(given)(fn)
}

trait LiftLike[S, F[_]] {
  implicit val typeState: TypeTagTree[S]
  implicit val state: S
  implicit val op: LiftOp[S, F]
}

@implicitNotFound("Unable to find a fluxmuster converter for ${F} -> ${G}")
trait ->[F[_], G[_]] {
  implicit def apply[A](a: F[A]): G[A]
}

trait Converter[F[_], G[_]] extends (F -> G)

sealed trait LiftedAndFlatten[A, D, S, F[_]] extends LiftLike[S, F] with StepLike[A, F[D], F[D], F[D]] with Runner[A, F[D], F[D], F[D]] {
  val name = "|>"

  implicit lazy val typeMappedDownstream = typeMappedUpstream
  implicit lazy val typeAcceptUpstream = typeMappedUpstream

  private def runInThisContext[G[_]](runner: A => G[D])(implicit converter: G -> F): LinkDownstream[A, F[D]] =
    (a: A) => {
      val runOtherInThisContext: A => F[G[D]] = op.apply(runner)(state)
      val resultAfterRunning: F[G[D]] = runOtherInThisContext(a)

      //flatMap!
      val mapResultBackIntoThisContext = op.map(resultAfterRunning)(converter.apply)(state)
      val flattenedBackIntoThisContext: F[D] = op.flatten(mapResultBackIntoThisContext)(state)
      flattenedBackIntoThisContext
    }

  def lift[U, V, W, G[_]](other: Lifted[A, U, V, G[D], W, G])(implicit e1: U <:< V, converter: G -> F): LiftedAndFlatten[A, D, S, F] =
    LiftedAndFlatten[A, D, S, F](runInThisContext(other.run), state, op)(typeAcceptDownstream, typeMappedUpstream, typeState)

  def lift[U, V, W, G[_]](other: LiftedAndFlatten[A, D, W, G])(implicit converter: G -> F): LiftedAndFlatten[A, D, S, F] =
    LiftedAndFlatten[A, D, S, F](runInThisContext(other.run), state, op)(typeAcceptDownstream, typeMappedUpstream, typeState)
}

object LiftedAndFlatten {
  private case class Builder[A, D, S, F[_]](downstream: LinkDownstream[A, F[D]], upstream: LinkUpstream[F[D], F[D]], state: S, op: LiftOp[S, F])(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedUpstream: TypeTagTree[F[D]], val typeState: TypeTagTree[S]) extends LiftedAndFlatten[A, D, S, F]

  def apply[A, D, S, F[_]](downstream: LinkDownstream[A, F[D]], state: S, op: LiftOp[S, F])(implicit typeAcceptDownstream: TypeTagTree[A], typeMappedUpstream: TypeTagTree[F[D]], typeState: TypeTagTree[S]): LiftedAndFlatten[A, D, S, F] =
    Builder[A, D, S, F](downstream, identity[F[D]], state, op)
}

trait Lifted[A, B, C, D, S, F[_]] extends LiftLike[S, F] with StepLike[A, B, C, D] with Runner[A, B, C, D] {
  val name = "|>"

  def lift[T, U](other: Step[A, T, U, D])(implicit tFofT: TypeTagTree[F[T]], tFofD: TypeTagTree[F[D]]): Lifted[A, T, U, D, S, F] =
    Lifted(other, state, op)

  def lift[U, V, W, G[_]](other: Lifted[A, U, V, D, W, G])(implicit e1: U <:< V, e2: G[_] <:< F[_], tFofD: TypeTagTree[F[D]], tFofFofD: TypeTagTree[F[F[D]]]): LiftedAndFlatten[A, D, S, F] = {
//    val appliedLiftOp = new LiftOp[S, F] {
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

    val down: LinkDownstream[A, F[D]] =
      op.apply(other.run)(state)

    val up: LinkUpstream[F[D], F[D]] =
      identity

    LiftedAndFlatten[A, D, S, F](down, state, op)
  }
}

//Note: do not refactor to provide state at execution time because it's very possible
//      that different parts of the pipeline will require different states and the # of
//      needed states would depend on what's in the pipeline. So either we avoid it
//      altogether by capturing the state at definition time or we create a registry
//      that can be used as a lookup by the members of the pipeline. We could indeed do
//      both by having the registry be the state!

object Lifted {
  private case class Builder[A, B, C, D, S, F[_]](downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], state: S, op: LiftOp[S, F], connections: immutable.Seq[Step[_, _, _, _]] = immutable.Seq())(implicit val typeAcceptDownstream: TypeTagTree[A], val typeMappedDownstream: TypeTagTree[B], val typeAcceptUpstream: TypeTagTree[C], val typeMappedUpstream: TypeTagTree[D], val typeState: TypeTagTree[S]) extends Lifted[A, B, C, D, S, F]

  def apply[A, B, C, D, S, F[_]](downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D], state: S, op: LiftOp[S, F], connections: immutable.Seq[Step[_, _, _, _]] = immutable.Seq())(implicit typeAcceptDownstream: TypeTagTree[A], typeMappedDownstream: TypeTagTree[B], typeAcceptUpstream: TypeTagTree[C], typeMappedUpstream: TypeTagTree[D], typeState: TypeTagTree[S]): Lifted[A, B, C, D, S, F] =
    Builder(downstream, upstream, state, op)

  def apply[A, B, C, D, S, F[_]](step: Step[A, B, C, D], state: S, op: LiftOp[S, F])(implicit typeState: TypeTagTree[S], typeLiftIntoBridge: TypeTagTree[F[B]], typeLiftIntoUpstream: TypeTagTree[F[D]]): Lifted[A, B, C, D, S, F] =
    Builder[A, B, C, D, S, F](step.downstream, step.upstream, state, op, immutable.Seq(step))(step.typeAcceptDownstream, step.typeMappedDownstream, step.typeAcceptUpstream, step.typeMappedUpstream, typeState)

//  def apply[A, B, C, D, F[_]](step: Step[A, B, C, D])(liftWith: (A => D) => (A => F[D]))(implicit typeState: TypeTagTree[Nothing], typeLiftIntoBridge: TypeTagTree[F[B]], typeLiftIntoUpstream: TypeTagTree[F[D]]): Lifted[A, B, C, D, Nothing, F] =
//    Builder[A, B, C, D, Nothing, F](step, new StatelessLiftOp[F] {
//      override def apply[T, U](runner: T => D): (A) => F[D] = liftWith(runner)
//    })
}

import scala.concurrent._
object Implicits {
  implicit object FutureLiftOp extends LiftOp[ExecutionContext, Future] {
    def apply[A, D](runner: A => D)(implicit ec: ExecutionContext): A => Future[D] = {
      (a: A) =>
        future {
          runner(a)
        }
    }

    def map[A, B](given: Future[A])(fn: A => B)(implicit ec: ExecutionContext): Future[B] =
      given.map(fn)(ec)

    def flatten[A](given: Future[Future[A]])(implicit ec: ExecutionContext): Future[A] = {
      import scala.util._
      val p = Promise[A]()
      given.onComplete {
        case Success(next) =>
          p.completeWith(next)
        case Failure(t) =>
          p.failure(t)
      }
      p.future
    }
  }

  //Courtesy Miles Sabin:
  //  http://www.chuusai.com/2012/05/10/shapeless-polymorphic-function-values-2/

  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  implicit def polyToMono2[G[_], T](f: Id -> G): T => G[T] = f(_)
  implicit def polyToMono3[F[_], T](f: F -> Id): F[T] => T = f(_)
  implicit def polyToMono4[T](f: Id -> Id) : T => T = f[T](_)
  implicit def polyToMono5[F[_], G, T](f: F -> Const[G]#λ) : F[T] => G = f(_)
  implicit def polyToMono6[G, T](f: Id -> Const[G]#λ) : T => G = f(_)

  implicit object IdentityConverter extends (Id -> Id) {
    implicit def apply[A](a: Id[A]): Id[A] = a
  }

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }
}

object BackToTheFuture {
  def apply[A, B, C, D](step: Step[A, B, C, D])(implicit ec: ExecutionContext, liftOp: LiftOp[ExecutionContext, Future], tFofB: TypeTagTree[Future[B]], tFofD: TypeTagTree[Future[D]]): Lifted[A, B, C, D, ExecutionContext, Future] =
    Lifted(step, ec, liftOp)
}

object Test extends App {
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import Implicits._

  val IntIdentity = Identity[Int, Int]
  val LongIdentity = Identity[Long, Long]
  val IntLongIdentity = Identity[Int, Long]
  val IntStringIdentity = Identity[Int, String]
  val IntLongMap = DownAndUp((x: Int) => x, (x: Int) => x.toLong)
  val IntLongStringMap = DownAndUp((x: Int) => x, (x: Long) => x.toString)

  val IntPrintIdentity = DownAndUp({x: Int => println(s"[${Thread.currentThread().getName}] Downstream: $x"); x}, {x: Int => println(s"[${Thread.currentThread().getName}] Upstream: $x"); x})
  val IntStringPrintIdentity = DownAndUp({x: Int => println(s"[${Thread.currentThread().getName}] Downstream: $x"); x.toString}, {x: String => println(s"[${Thread.currentThread().getName}] Upstream: $x"); x})

  val LiftedIntIdentity = BackToTheFuture(IntIdentity)
  val LiftedIntStringIdentity = BackToTheFuture(IntStringIdentity)
  val LiftedIntLongStringMap = BackToTheFuture(IntLongStringMap)
  val LiftedIntStringPrintIdentity = BackToTheFuture(IntStringPrintIdentity)
  val LiftedLifted = LiftedIntStringIdentity lift LiftedIntStringPrintIdentity

  val a1 = DownAndUp({x: Int => x.toString}, {x: Int => x.toLong + 2})
  val a2 = DownAndUp({x: String => x.toInt + 1}, {x: Int => x + 2})

  val a3: Step[Int, Int, Long, Long] = Identity[Int, Long]
  val a4: Step[Int, Int, Int, Long] = a1 combine a2
  val l1: Lifted[Int, Int, Int, Long, ExecutionContext, Future] = BackToTheFuture(a4)
  //val l1step: Step[Int, Int, Int, Long] = l1.step
  val foo = a3 combine a4
  //val a5 = a3 combine l1
  //val l2 = l1 combine IntPrintIdentity
  //val l2 = LiftedFuture(a5)
  val f1 = LiftedLifted(0)

  val ff1: Lifted[Int, Int, Int, Long, ExecutionContext, Future] = BackToTheFuture(a1 combine a2 combine IntIdentity combine IntIdentity)
  val ff2: Lifted[Int, Int, Long, Long, ExecutionContext, Future] = BackToTheFuture(IntLongIdentity)
  val ff3 = ff2 lift ff1
  val ff4 = ff3 lift ff3
  val fff = ff4(0)

  val result = Await.result(fff, 10.seconds)
  println(result)
}
