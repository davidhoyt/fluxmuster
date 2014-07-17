package com.github.davidhoyt.fluxmuster

object Upstream {
  val NAME = Macros.nameOf[Upstream.type]

  def apply[A, C, D](onUpstream: LinkUpstream[C, D]): ProxySpecification[A, A, C, D] =
    apply(NAME)(onUpstream)

  def apply[A, C, D](name: String)(onUpstream: LinkUpstream[C, D]): ProxySpecification[A, A, C, D] =
    ProxySpecification(name)(identity, onUpstream)
}
