package com.github.davidhoyt.fluxmuster

object BiDirectional {
  val NAME = Macros.nameOf[BiDirectional.type]

  def apply[A, B, C, D](onDownstream: LinkDownstream[A, B])(onUpstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, B, C, D] =
    apply(NAME)(onDownstream)(onUpstream)(tA, tB, tC, tD)

  def apply[A, B, C, D](name: String)(onDownstream: LinkDownstream[A, B])(onUpstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, B, C, D] =
    ProxyStep(Metadata(name, tA, tB, tC, tD))(onDownstream, onUpstream)
}
