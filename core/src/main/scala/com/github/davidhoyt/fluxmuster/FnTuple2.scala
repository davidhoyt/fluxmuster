package com.github.davidhoyt.fluxmuster

object FnTuple2 {
  val NAME = Macros.nameOf[FnTuple2.type]

  def apply[A, B, C, D](t: (A => B, C => D))(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, B, C, D] =
    apply(s"$NAME($t)")(t)(tA, tB, tC, tD)

  def apply[A, B, C, D](name: String)(t: (A => B, C => D))(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, B, C, D] = {
    val (downstream, upstream) = t
    ProxySpecification(Metadata(name, tA, tB, tC, tD))(downstream, upstream)
  }
}
