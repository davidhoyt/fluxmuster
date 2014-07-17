package com.github.davidhoyt.fluxmuster

object Downstream {
  val NAME = Macros.nameOf[Downstream.type]

  def apply[A, B, C](onDownstream: LinkDownstream[A, B]): ProxySpecification[A, B, C, C] =
    apply(NAME)(onDownstream)

  def apply[A, B, C](name: String)(onDownstream: LinkDownstream[A, B]): ProxySpecification[A, B, C, C] =
    ProxySpecification(name)(onDownstream, identity)
}
