package com.github.davidhoyt.fluxmuster



object Downstream {
  val NAME = Macros.nameOf[Downstream.type]

  def apply[A, B, C](onDownstream: LinkDownstream[A, B]): ProxySpecification[A, B, C, C] =
    apply(NAME)(onDownstream)

  def apply[A : TypeData, B : TypeData, C : TypeData](name: String)(onDownstream: LinkDownstream[A, B]): ProxySpecification[A, B, C, C] = {
    val a = implicitly[TypeData[A]]
    val b = implicitly[TypeData[B]]
    val c = implicitly[TypeData[C]]
    //val d = implicitly[TypeData[D]]
    println(a)
    ProxySpecification(name)(onDownstream, identity)
  }
}
