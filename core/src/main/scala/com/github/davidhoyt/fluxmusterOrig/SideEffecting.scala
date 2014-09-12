package com.github.davidhoyt.fluxmusterOrig

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

object SideEffecting {
  val NAME = Macros.nameOf[SideEffecting.type]

  private def doNothing[A](a: A): Unit = ()

  def apply[A, B](name: String)(onDownstream: LinkDownstream[A, Unit])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxyStep[A, A, B, B] =
    apply[A, B](name = name, onDownstream = onDownstream)(tA, tB)

  def apply[A, B](name: String = NAME, onDownstream: LinkDownstream[A, Unit] = doNothing[A] _, onUpstream: LinkUpstream[B, Unit] = doNothing[B] _)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxyStep[A, A, B, B] =
    ProxyStep(Metadata(name, tA, tA, tB, tB))({ a => onDownstream(a); a}, { b => onUpstream(b); b})
}
