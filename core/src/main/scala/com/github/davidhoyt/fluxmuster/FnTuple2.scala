package com.github.davidhoyt.fluxmuster

object FnTuple2 {
  val NAME = Macros.nameOf[FnTuple2.type]

  def apply[A, B, C, D](t: (A => B, C => D)): ProxySpecification[A, B, C, D] =
    apply(s"$NAME($t)")(t)

  def apply[A, B, C, D](name: String)(t: (A => B, C => D)): ProxySpecification[A, B, C, D] = {
    val (downstream, upstream) = t
    ProxySpecification(name)(downstream, upstream)
  }
}
