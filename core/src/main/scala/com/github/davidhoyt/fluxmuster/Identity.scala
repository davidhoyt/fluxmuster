package com.github.davidhoyt.fluxmuster

object Identity {
  val NAME = Macros.nameOf[Identity.type]

  def apply[A, B](implicit tA: TypeData[A], tB: TypeData[B]): ProxySpecification[A, A, B, B] =
    apply(NAME)(tA, tB)

  def apply[A, B](name: String)(implicit tA: TypeData[A], tB: TypeData[B]): ProxySpecification[A, A, B, B] =
    ProxySpecification(Metadata(name, tA, tA, tB, tB))(identity, identity)
}
