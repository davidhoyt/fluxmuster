package com.github.davidhoyt.fluxmuster

object Identity {
  val NAME = Macros.nameOf[Identity.type]

  def apply[A, B]: ProxySpecification[A, A, B, B] =
    apply(NAME)

  def apply[A, B](name: String): ProxySpecification[A, A, B, B] =
    ProxySpecification(name)(identity, identity)
}
