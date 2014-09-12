package com.github.davidhoyt.fluxmuster



object Downstream {
  val NAME = Macros.simpleNameOf[Downstream.type]

  def apply[A, B, C](onDownstream: LinkDownstream[A, B])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxyStep[A, B, C, C] =
    apply(NAME)(onDownstream)(tA, tB, tC)

  def apply[A, B, C](name: String)(onDownstream: LinkDownstream[A, B])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxyStep[A, B, C, C] =
    ProxyStep(Metadata(name, tA, tB, tC, tC, lifted = false, asString = s"$name[${tA.toShortString}, ${tB.toShortString}, ${tC.toShortString}]"))(onDownstream, identity)
}
