package com.github.davidhoyt.fluxmuster

import scala.util.Try

trait Proxy[-TAcceptDownstream, +TMappedDownstream, -TAcceptUpstream, +TMappedUpstream]
extends ProxySpecification[TAcceptDownstream, TMappedDownstream, TAcceptUpstream, TMappedUpstream] {
  lazy val downstream = specification.downstream
  lazy val upstream = specification.upstream

  override def toString =
    s"Proxy(${metadata.mkString(" <~> ")})"

  val metadata: ConnectedMetadata
  val specification: ProxySpecification[TAcceptDownstream, TMappedDownstream, TAcceptUpstream, TMappedUpstream]
  def apply(value: TAcceptDownstream): TMappedUpstream
}

object Proxy {
  def apply[A, B, C](providedSpec: => ProxySpecification[A, B, B, C]): Proxy[A, B, B, C] =
    new Proxy[A, B, B, C] {
      lazy val specification =
        providedSpec

      lazy val metadata: ConnectedMetadata =
        specification.metadata

      def apply(value: A): C =
        ProxySpecification.run(specification)(value)
    }
}
