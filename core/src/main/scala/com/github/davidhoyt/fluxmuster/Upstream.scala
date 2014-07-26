package com.github.davidhoyt.fluxmuster

object Upstream {
  val NAME = Macros.simpleNameOf[Upstream.type]

  def apply[A, C, D](onUpstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, A, C, D] =
    apply(NAME)(onUpstream)(tA, tC, tD)

  def apply[A, C, D](name: String)(onUpstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, A, C, D] =
    ProxySpecification(Metadata(name, tA, tA, tC, tD, s"$name[${tA.toShortString}, ${tC.toShortString}, ${tD.toShortString}]"))(identity, onUpstream)
}
