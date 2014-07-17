package com.github.davidhoyt.fluxmuster

object Identity {
  def apply[A, B]: ProxySpecification[A, A, B, B] =
    ProxySpecification(Macros.nameOf[Identity.type])(identity, identity)
}
