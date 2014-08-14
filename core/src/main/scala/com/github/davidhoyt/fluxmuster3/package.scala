package com.github.davidhoyt

package object fluxmuster3 {
  import scala.collection._
  import scala.concurrent.{ExecutionContext, Future}
  import com.github.davidhoyt.fluxmuster.TypeTagTree

  import scala.language.higherKinds
  import scala.language.implicitConversions

  type Downstream[In, Out] =
    Link[In, Out]

  type Upstream[In, Out] =
    Link[In, Out]

  type ProjectedLift[In, Out, State, Into[_]] =
    Lift[In, Into[Out], Into[Out], Out, State, Into]

  type ChainableLink =
    Link[_, _]

  type ChainableStep =
    StepLike[_, _, _, _] with StepRun[_, _, _, _] with StepChaining with ChainRun[_, _, _, _] with Named

  type ChainableLift =
    ChainRun[_, _, _, _]

  type ChainLink = immutable.Seq[ChainableLink]
  val EmptyChainLink = immutable.Seq[ChainableLink]()

  type ChainStep = immutable.Seq[ChainableStep]
  val EmptyChainStep = immutable.Seq[ChainableStep]()

  type ChainLift = immutable.Seq[ChainableLift]
  val EmptyChainLift = immutable.Seq[ChainableLift]()

  type FnChainLink =
    (ChainableLink, ChainLink, ChainLink) => ChainLink

  type FnChainStep =
    (ChainableStep, ChainStep, ChainStep) => ChainStep

  implicit object IdentityConverter {
    def apply[F[_]] = new Converter[F, F] {
      override implicit def apply[A](a: F[A]): F[A] = a
    }
  }

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  implicit class LinkEnhancements[In, Out](val link: Link[In, Out]) extends AnyVal {
    def toLink: Link[In, Out] =
      link
  }

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
      Link(fn)

    def link[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit proofMyOutputCanBeOtherIn: Out => OtherIn, tIn: TypeTagTree[In], tOut: TypeTagTree[Out], tOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
      toLink(tIn, tOut).andThen(other)(proofMyOutputCanBeOtherIn)

    def toStep(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Step[In, Out, Out, Out] =
      Step.downstream(fn)
  }

  implicit def functionToLink[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    fn.toLink(tIn, tOut)

  implicit class FunctionTupleEnhancements[A, B, C, D](val tuple: (A => B, C => D)) extends AnyVal {
    def toStep(implicit proofDownstreamCanMapToUpstream: B => C, tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D] = {
      val (downstream, upstream) = tuple
      Step(downstream)(upstream)
    }
  }

  implicit class LinkTupleEnhancements[A, B, C, D](val tuple: (Link[A, B], Link[C, D])) extends AnyVal {
    def toStep(implicit proofDownstreamCanMapToUpstream: B => C): Step[A, B, C, D] = {
      val (downstream, upstream) = tuple
      Step(downstream)(upstream)
    }
  }

  implicit object FutureLiftOps extends LiftOps[ExecutionContext, Future] {
    import scala.concurrent.{Promise, future}
    import scala.util.control.NonFatal

    def liftRunner[A, B, C, D](runner: ChainRun[A, B, C, D])(implicit ec: ExecutionContext, chain: ChainLift, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => Future[D] =
      (a: A) =>
        future {
          runner.runChain(a)
        }

    def point[A](given: => A)(implicit ec: ExecutionContext): Future[A] =
      try Future.successful(given)
      catch {
        case NonFatal(error) =>
          Future.failed(error)
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
}
