package com.github.davidhoyt

import scala.collection._

package object fluxmuster {
  //type Meta = Metadata
  type Error = Throwable

  type ErrorProcessor = Error => Unit

  type LinkDownstream[-TAcceptDownstream, +TMappedDownstream] = TAcceptDownstream => TMappedDownstream
  type LinkUpstream[-TAcceptUpstream, +TMappedUpstream] = TAcceptUpstream => TMappedUpstream

  type ConnectedMetadata = immutable.Seq[Metadata]

  def identity[X](x: X): X = x

  /**
   *
   * @tparam A Expected accepted downstream type of connected specification
   * @tparam B Expected downstream type of connected specification that will be mapped from `A`
   * @tparam C Expected upstream type of connected specification that will be provided after
   *           downstream processing is complete and is coming back upstream
   * @tparam D Expected upstream type of connected specification that will be mapped from `C`
   *           when the value is coming back upstream
   * @tparam E Type that is accepted for processing downstream
   * @tparam F Type that will be mapped to downstream given type `A`
   * @tparam G Type that will be provided after downstream processing is complete and
   *           is coming back upstream
   * @tparam H Type that will be mapped to upstream given type `C`
   */
  type ProxyLift[A, B, C, D, E, F, G, H] =
    ProxySpecification[A, B, C, D] => ProxySpecification[E, F, G, H]

  import scala.language.implicitConversions

//  implicit def toProxyLift[A, B, C, D, E, F, G, H](fn: FnProxyLift[A, B, C, D, E, F, G, H]): ProxyLift[A, B, C, D, E, F, G, H] =
//    new ProxyLift[A, B, C, D, E, F, G, H] {
//      def apply(p: ProxySpecification[A, B, C, D]) =
//        fn(p)
//    }

  implicit class ProxyLiftEnhancements[A, B, C, D, E, F, G, H](val proxy: ProxyLift[A, B, C, D, E, F, G, H]) extends AnyVal {
    def |>(p2: ProxySpecification[A, B, C, D]): ProxySpecification[E, F, G, H] =
      lift(p2)

    def lift(p2: ProxySpecification[A, B, C, D]): ProxySpecification[E, F, G, H] =
      proxy(p2)
  }

  implicit def functionToDownstreamProxySpecification[A, B, C](fn: A => B)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxySpecification[A, B, C, C] =
    Downstream[A, B, C](fn)(tA, tB, tC)

  implicit class Function1ConnectEnhancements[A, B](val fn: A => B) extends AnyVal {
    def <~>[C, D, E](p2: ProxySpecification[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D], tE: TypeTagTree[E]): ProxySpecification[A, C, D, E] =
      connect(p2)(tA, tB, tC, tD, tE)

    def connect[C, D, E](p2: ProxySpecification[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D], tE: TypeTagTree[E]): ProxySpecification[A, C, D, E] =
      Downstream[A, B, E](fn)(tA, tB, tE) connect p2
  }

  implicit class Function1BiDirectionalEnhancements[A, B](val fn: A => B) extends AnyVal {
    def <~[C, D](upstreamNext: LinkUpstream[C, A])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[D, D, C, B] =
      upstream(upstreamNext)(tA, tB, tC, tD)

    def upstream[C, D](upstreamNext: LinkUpstream[C, A])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[D, D, C, B] =
      Upstream[D, A, B](fn)(tD, tA, tB) connect Upstream[D, C, A](upstreamNext)(tD, tC, tA)

    def ~>[C, D](next: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, C, D, D] =
      downstream(next)(tA, tB, tC, tD)

    def downstream[C, D](next: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, C, D, D] =
      Downstream[A, B, D](fn)(tA, tB, tD) connect Downstream[B, C, D](next)(tB, tC, tD)
  }

  implicit def tuple2Function1ToProxySpecification[A, B, C, D](t: (A => B, C => D))(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, B, C, D] =
    FnTuple2(t)(tA, tB, tC, tD)

  implicit class Tuple2Function1Enhancements[A, B, E, F](val t: (A => B, E => F)) extends AnyVal {
    def <~>[C, D](p2: ProxySpecification[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tE: TypeTagTree[E], tF: TypeTagTree[F]): ProxySpecification[A, C, D, F] =
      connect(p2)(tA, tB, tE, tF)

    def connect[C, D](p2: ProxySpecification[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tE: TypeTagTree[E], tF: TypeTagTree[F]): ProxySpecification[A, C, D, F] =
      FnTuple2(t)(tA, tB, tE, tF) <~> p2
  }

  implicit def functionToProxySpecification[A, B, C, D](fn: A => B)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxySpecification[A, B, B, B] =
    ProxySpecification(immutable.Seq(Metadata(fn.toString(), tA, tB, tB, tB)), fn, identity)

  implicit class ProxySpecificationConnectEnhancements[A, B, G, E](val p1: ProxySpecification[A, B, G, E]) extends AnyVal {
    def <~>[C, F](p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] =
      connect(p2)

    def connect[C, F](p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] =
      ProxySpecification.combine(p1, p2)

    def <~>[C, F](link: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tG: TypeTagTree[G], tE: TypeTagTree[E]): ProxySpecification[A, C, G, E] =
      connect(link)(tA, tC, tG, tE)

    def connect[C, F](link: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tG: TypeTagTree[G], tE: TypeTagTree[E]): ProxySpecification[A, C, G, E] = {
      val spec = ProxySpecification(immutable.Seq(Metadata(link.toString(), tA, tC, tG, tE)), link, identity[G])
      val combined = ProxySpecification.combine(p1, spec)
      combined
    }
  }

  implicit class ProxySpecificationBiDirectionalEnhancements[A, B, C, D](val spec: ProxySpecification[A, B, C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]) {
    def <~[E](onUpstream: LinkUpstream[E, C])(implicit tE: TypeTagTree[E]): ProxySpecification[A, B, E, D] =
      spec connect Upstream[B, E, C](onUpstream)(tB, tE, tC)

    def ~>[E](onDownstream: LinkDownstream[B, E])(implicit tE: TypeTagTree[E]): ProxySpecification[A, E, C, D] =
      spec connect Downstream[B, E, C](onDownstream)(tB, tE, tC)
  }

  implicit class ConnectedMetadataEnhancements(val connectedMetadata: ConnectedMetadata) extends AnyVal {
    def toShortString = {
      val sb = StringBuilder.newBuilder
      for ((meta, idx) <- connectedMetadata.zipWithIndex) {
        if (idx > 0)
          sb ++= ", "
        sb ++= meta.toShortString
      }
      sb.toString()
    }
  }

  trait Logger {
    def warn(message: => String): Unit
    def error(message: => String): Unit
  }

  implicit object ConsoleLogger extends Logger {
    def warn(message: => String) =
      println(s"[WARN] $message")

    def error(message: => String) =
      println(s"[ERROR] $message")
  }

//  implicit val DefaultHystrixConfiguration =
//    HystrixConfiguration("<default>", "<unknown>")

  def errorProcessor(error: Error)(implicit logger: Logger): Unit =
    logger.error(error.getMessage)

  val defaultErrorProcessor = errorProcessor _
}
