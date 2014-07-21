package com.github.davidhoyt.fluxmuster

object Upstream {
  val NAME = Macros.nameOf[Upstream.type]

  def apply[A, C, D](onUpstream: LinkUpstream[C, D])(implicit tA: TypeData[A], tC: TypeData[C], tD: TypeData[D]): ProxySpecification[A, A, C, D] =
    apply(NAME)(onUpstream)(tA, tC, tD)

  def apply[A, C, D](name: String)(onUpstream: LinkUpstream[C, D])(implicit tA: TypeData[A], tC: TypeData[C], tD: TypeData[D]): ProxySpecification[A, A, C, D] =
    ProxySpecification(Metadata(name, tA, tA, tC, tD))(identity, onUpstream)
}
