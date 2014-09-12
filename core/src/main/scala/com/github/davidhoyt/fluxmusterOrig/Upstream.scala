package com.github.davidhoyt.fluxmusterOrig

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

object Upstream {
  val NAME = Macros.simpleNameOf[Upstream.type]

  def apply[A, C, D](onUpstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, A, C, D] =
    apply(NAME)(onUpstream)(tA, tC, tD)

  def apply[A, C, D](name: String)(onUpstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, A, C, D] =
    ProxyStep(Metadata(name, tA, tA, tC, tD, lifted = false, asString = s"$name[${tA.toShortString}, ${tC.toShortString}, ${tD.toShortString}]"))(identity, onUpstream)
}
