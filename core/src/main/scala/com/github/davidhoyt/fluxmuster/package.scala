package com.github.davidhoyt

import scala.collection._

package object fluxmuster {
  type Metadata = String
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
    def |>(p2: ProxySpecification[A, B, C, D]): ProxySpecification[E, F, G, H] = lift(p2)
    def lift(p2: ProxySpecification[A, B, C, D]): ProxySpecification[E, F, G, H] =
      proxy(p2)
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
