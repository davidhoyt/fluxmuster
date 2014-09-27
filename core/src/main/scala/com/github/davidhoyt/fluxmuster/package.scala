package com.github.davidhoyt

package object fluxmuster {
  import scala.concurrent.{ExecutionContext, Future}
  import scala.util.Try
  import Chains._

  import scala.language.higherKinds
  import scala.language.existentials
  import scala.language.implicitConversions

  type Downstream[In, Out] =
    Link[In, Out]

  type Upstream[In, Out] =
    Link[In, Out]

  /**
   * An arrow that maps between a domain `From` and its codomain `To`.
   *
   * This is an alias for `->`.
   */
  type Converter[From[_], To[_]] = From -> To

  implicit val EmptyUnit: Unit = ()

  type LinkAny =
    Link[_, _]

  type ProxyAny =
    Proxy[_, _, _, _]

  type LiftOpsAny =
    LiftOps[_ >: Any <: Any, Into forSome { type Into[_] }]

  type LiftChainEntryAny =
    LiftChainEntry[_, _, _, From forSome { type From[_] }, Into forSome { type Into[_] }]

  type LinkExistential =
    Link[_ >: Any <: Any, _ >: Any <: Any]

  type SideEffecting[Out] =
    Out => Unit

  def typeTagTreeOf[T](implicit ttt: TypeTagTree[T]) =
    TypeTagTree.typeTagTreeOf[T](ttt)

  implicit def functionToLink[In, Out](fn: In => Out)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
    Link(fn)

  implicit def proxyToLinkedProxy[A, B, C, D](proxy: Proxy[A, B, C, D])(implicit proofDownstreamCanMapToUpstream: B => C): LinkedProxy[A, B, C, D] =
    proxy.linked(proofDownstreamCanMapToUpstream)

  implicit def linkToLinkedProxy[A, D](link: Link[A, D]): LinkedProxy[A, A, D, D] =
    link.toLinkedProxy

  implicit class LinkEnhancements[In, Out](val link: Link[In, Out]) extends AnyVal {
    def toLink: Link[In, Out] =
      link
  }

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(name: String)(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
      Link(name)(fn)

    def toLink(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): Link[In, Out] =
      Link(fn)

    def link[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit proofMyOutputCanBeOtherIn: Out => OtherIn, typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out], typeOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
      toLink andThen other

    def toLinkedProxy(implicit typeIn: TypeTagTree[In], typeOut: TypeTagTree[Out]): LinkedProxy[In, In, Out, Out] =
      toLink.toLinkedProxy
  }

  implicit class LinkChainEnhancements(val linkChain: LinkChain) extends AnyVal {
    def asDefaultString =
      linkChain.map(_.asDefaultString).mkString(", ")

    def asShortString =
      linkChain.map(_.asShortString).mkString(", ")

    def runLinkChainAny(in: Any): Any =
      linkChain.foldLeft(in) {
        case (soFar, next) =>
          next.runAny(soFar)
      }
  }

  implicit object IdentityConverter {
    def apply[F[_]] = new Converter[F, F] {
      override implicit def apply[A](a: F[A]): F[A] = a
    }
  }

  implicit object TryConverter extends (Try -> Try) {
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    private val logger = Logger(LoggerFactory.getLogger(Macros.nameOf[TryConverter.type]))

    implicit def apply[A](t: Try[A]): Try[A] = {
      logger.debug(s"Applying conversion Try -> Try")
      t
    }
  }

  implicit object FutureConverter extends (Future -> Future) {
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    private val logger = Logger(LoggerFactory.getLogger(Macros.nameOf[FutureConverter.type]))

    implicit def apply[A](f: Future[A]): Future[A] = {
      logger.debug(s"Applying conversion Future -> Future")
      f
    }
  }

  implicit object TryFutureConverter extends (Try -> Future) {
    import scala.util.{Success, Failure}
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory

    private val logger = Logger(LoggerFactory.getLogger(Macros.nameOf[TryFutureConverter.type]))

    implicit def apply[A](t: Try[A]): Future[A] = {
      logger.debug(s"Applying conversion Try -> Future")

      t match {
        case Success(a) =>
          Future.successful(a)
        case Failure(t) =>
          Future.failed(t)
      }
    }
  }

  implicit object FutureLiftOps extends LiftOps[ExecutionContext, Future] {
    import com.typesafe.scalalogging.Logger
    import org.slf4j.LoggerFactory
    import scala.concurrent.Promise
    import scala.util.control.NonFatal

    private val logger = Logger(LoggerFactory.getLogger(Macros.nameOf[FutureLiftOps.type]))

    def liftRunner[A, D](linkChain: LinkChain, opsChain: ChainedLiftOps[Future], runner: A => D)(implicit ec: ExecutionContext, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Future[D] =
      (a: A) =>
        Future {
          runner(a)
        }

    def point[A](given: => A): Future[A] =
      try Future.successful(given)
      catch {
        case NonFatal(error) =>
          Future.failed(error)
      }

    def map[A, B](given: Future[A])(fn: A => B)(implicit ec: ExecutionContext): Future[B] = {
      logger.debug(s"Mapping with: $given")
      val r = given.map(fn)(ec)
      logger.debug(s"Result of map: $r")
      r
    }

    def flatten[A](given: Future[Future[A]])(implicit ec: ExecutionContext): Future[A] = {
      import scala.util._
      logger.debug(s"Flattening with: $given")
      val p = Promise[A]()
      given.onComplete {
        case Success(next) =>
          p.completeWith(next)
        case Failure(t) =>
          p.failure(t)
      }
      val r = p.future
      logger.debug(s"Result of flattening: $r")
      r
    }
  }
}
