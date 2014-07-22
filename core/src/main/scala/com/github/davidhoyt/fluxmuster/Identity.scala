package com.github.davidhoyt.fluxmuster

object Identity {
  val NAME = Macros.nameOf[Identity.type]

  def apply[A, B](implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxySpecification[A, A, B, B] =
    apply(NAME)(tA, tB)

  def apply[A, B](name: String)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxySpecification[A, A, B, B] =
    ProxySpecification(Metadata(name, tA, tA, tB, tB))(identity, identity)
}
