package com.github.davidhoyt.fluxmuster

object Identity {
  val NAME = Macros.simpleNameOf[Identity.type]

  def apply[A, B](implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxyStep[A, A, B, B] =
    apply(NAME)(tA, tB)

  def apply[A, B](name: String)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxyStep[A, A, B, B] =
    ProxyStep(Metadata(name, tA, tA, tB, tB, s"$name[${tA.toShortString}, ${tB.toShortString}]"))(identity, identity)
}
