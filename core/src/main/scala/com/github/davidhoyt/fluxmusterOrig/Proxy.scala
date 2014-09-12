package com.github.davidhoyt.fluxmusterOrig

import scala.util.Try

trait Proxy[-TAcceptDownstream, +TMappedDownstream, -TAcceptUpstream, +TMappedUpstream]
extends ProxyStep[TAcceptDownstream, TMappedDownstream, TAcceptUpstream, TMappedUpstream] {
  lazy val downstream = step.downstream
  lazy val upstream = step.upstream
  lazy val connections = step.connections

  override def toString =
    s"Proxy(${metadata.toShortString})"

  val metadata: ConnectedMetadata
  val step: ProxyStep[TAcceptDownstream, TMappedDownstream, TAcceptUpstream, TMappedUpstream]
  def apply(value: TAcceptDownstream): TMappedUpstream
}

object Proxy {
  def apply[A, B, C](providedStep: => ProxyStep[A, B, B, C]): Proxy[A, B, B, C] =
    new Proxy[A, B, B, C] {
      lazy val step =
        providedStep

      lazy val metadata: ConnectedMetadata =
        step.metadata

      def apply(value: A): C =
        ProxyStep.run(step)(value)
    }
}
