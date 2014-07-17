package com.github.davidhoyt.fluxmuster

object BiDirectional {
  val NAME = Macros.nameOf[BiDirectional.type]

  def apply[A, B, C, D](onDownstream: LinkDownstream[A, B])(onUpstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(NAME)(onDownstream)(onUpstream)

  def apply[A, B, C, D](name: String)(onDownstream: LinkDownstream[A, B])(onUpstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    ProxySpecification(name)(onDownstream, onUpstream)
}
