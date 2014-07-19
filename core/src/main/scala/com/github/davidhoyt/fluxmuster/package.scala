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

  implicit def functionToDownstreamProxySpecification[A, B, C](fn: A => B): ProxySpecification[A, B, C, C] =
    ProxySpecification("<function>")(fn, identity)

  implicit class Function1Enhancements[-A, +B](val fn: A => B) extends AnyVal {
    def <~>[C, D, E](p2: ProxySpecification[B, C, D, E]): ProxySpecification[A, C, D, E] =
      connect(p2)

    def connect[C, D, E](p2: ProxySpecification[B, C, D, E]): ProxySpecification[A, C, D, E] =
      functionToDownstreamProxySpecification(fn) <~> p2
  }

  implicit def tuple2Function1ToProxySpecification[A, B, C, D](t: (A => B, C => D)): ProxySpecification[A, B, C, D] =
    FnTuple2(t)

  implicit class Tuple2Function1Enhancements[-A, +B, -E, +F](val t: (A => B, E => F)) extends AnyVal {
    def <~>[C, D](p2: ProxySpecification[B, C, D, E]): ProxySpecification[A, C, D, F] =
      connect(p2)

    def connect[C, D](p2: ProxySpecification[B, C, D, E]): ProxySpecification[A, C, D, F] =
      FnTuple2(t) <~> p2
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
