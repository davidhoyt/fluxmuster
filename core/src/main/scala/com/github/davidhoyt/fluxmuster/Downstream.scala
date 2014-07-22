package com.github.davidhoyt.fluxmuster



object Downstream {
  val NAME = Macros.nameOf[Downstream.type]

  def apply[A, B, C](onDownstream: LinkDownstream[A, B])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxySpecification[A, B, C, C] =
    apply(NAME)(onDownstream)(tA, tB, tC)

  def apply[A, B, C](name: String)(onDownstream: LinkDownstream[A, B])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxySpecification[A, B, C, C] = {
    println(tA)
    ProxySpecification(Metadata(name, tA, tB, tC, tC))(onDownstream, identity)
  }
}
