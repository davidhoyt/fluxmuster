package com.github.davidhoyt.fluxmuster

object SideEffecting {
  val NAME = Macros.nameOf[SideEffecting.type]

  private def doNothing[A](a: A): Unit = ()

  def apply[A, B](name: String)(onDownstream: LinkDownstream[A, Unit]): ProxySpecification[A, A, B, B] =
    apply(name = name, onDownstream = onDownstream)

  def apply[A, B](name: String = NAME, onDownstream: LinkDownstream[A, Unit] = doNothing[A] _, onUpstream: LinkUpstream[B, Unit] = doNothing[B] _): ProxySpecification[A, A, B, B] =
    ProxySpecification(name)({ a => onDownstream(a); a}, { b => onUpstream(b); b})
}
