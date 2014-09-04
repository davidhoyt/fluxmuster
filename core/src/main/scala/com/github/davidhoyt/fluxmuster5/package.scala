package com.github.davidhoyt

import com.github.davidhoyt.fluxmuster5.runner.RunnerOps

package object fluxmuster5 {
  import scala.collection._
  import scala.concurrent.{ExecutionContext, Future}
  import scala.util.Try
  import com.github.davidhoyt.fluxmuster.TypeTagTree

  import scala.language.higherKinds
  import scala.language.existentials
  import scala.language.implicitConversions

  type Downstream[In, Out] =
    Link[In, Out]

  type Upstream[In, Out] =
    Link[In, Out]

  type ChainableLink =
    Chained[_, _]

  type ChainLink = immutable.Vector[ChainableLink]
  val EmptyChainLink = immutable.Vector[ChainableLink]()

  type ChainableOps = RunnerOps[_, Into forSome { type Into[_] }]
  type ChainOps     = immutable.Vector[ChainableOps]
  val EmptyChainOps = immutable.Vector[ChainableOps]()

  def newChainOps(ops: ChainableOps*): ChainOps =
    if ((ops ne null) && ops.nonEmpty)
      immutable.Vector[ChainableOps](ops:_*)
    else
      EmptyChainOps

  type SideEffecting[Out] = Out => Unit
  type ChainSideEffects[Out] = immutable.Vector[SideEffecting[Out]]
  def EmptyChainSideEffects[Out] = immutable.Vector[SideEffecting[Out]]()

  type FnChainLink =
    (ChainableLink, ChainLink, ChainLink) => ChainLink

  val typeUnit =
    typeTagTreeOf[Unit]

  val typeAny =
    typeTagTreeOf[Any]

  val typeFutureOfAny =
    typeTagTreeOf[Future[Any]]

  val typeFutureOfUnit =
    typeTagTreeOf[Future[Unit]]

  val typeNothing =
    typeTagTreeOf[Nothing]

  val typeFutureOfNothing =
    typeTagTreeOf[Future[Nothing]]

  implicit object IdentityConverter {
    def apply[F[_]] = new Converter[F, F] {
      override implicit def apply[A](a: F[A]): F[A] = a
    }
  }

  implicit object TryConverter extends (Try -> Try) {
    implicit def apply[A](t: Try[A]): Try[A] = t
  }

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  implicit class LinkEnhancements[In, Out](val link: Link[In, Out]) extends AnyVal {
    def toLink: Link[In, Out] =
      link

    def toProxy: Proxy[In, In, Out, Out] =
      link.toProxy
  }

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(name: String)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
      Link(name)(fn)

    def toLink(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
      Link(fn)

    def link[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit proofMyOutputCanBeOtherIn: Out => OtherIn, tIn: TypeTagTree[In], tOut: TypeTagTree[Out], tOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
      toLink(tIn, tOut).andThen(other)(proofMyOutputCanBeOtherIn)

//    def toStep(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Step[In, Out, Out, Out] =
//      Step.downstream(fn)
  }

  implicit def functionToLink[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    fn.toLink(tIn, tOut)

  implicit class ChainLinkEnhancements(val chainLink: ChainLink) extends AnyVal {
    def asDefaultString =
      chainLink.map(_.asDefaultString).mkString(", ")

    def asShortString =
      chainLink.map(_.asShortString).mkString(", ")
  }

  implicit class Tuple2Enhancements[A, B, C, D](val t: ((Downstream[A, B], Upstream[C, D]))) extends AnyVal {
    implicit def toProxy(implicit proof: B => C): Proxy[A, B, C, D] = {
      val (down, up) = t
      Proxy("<~>", down, up, proof)(down.typeOut, up.typeIn)
    }

    implicit def toProxyNeedsProof: ProxyNeedsProof[A, B, C, D] = {
      val (down, up) = t
      Proxy("<~>", down, up)
    }
  }

  implicit def proxyNeedsProofToProxy[A, B, C, D](proxy: ProxyNeedsProof[A, B, C, D])(implicit proofDownstreamCanMapToUpstream: B => C): Proxy[A, B, C, D] =
    proxy.withProof(proofDownstreamCanMapToUpstream)

  implicit def linkToProxy[A, D](link: Link[A, D]): Proxy[A, A, D, D] =
    link.toProxy

  def typeTagTreeOf[T](implicit ttt: TypeTagTree[T]) =
    TypeTagTree.typeTagTreeOf[T](ttt)

  implicit object FutureRunnerOps extends RunnerOps[ExecutionContext, Future] {
    import scala.concurrent.Promise
    import scala.util.control.NonFatal

    def liftRunner[A, D](chain: ChainLink, runner: A => D)(implicit ec: ExecutionContext, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] =
      (a: A) =>
        Future {
          runner(a)
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
