package com.github.davidhoyt.fluxmuster

object BiDirectional {
  val NAME = Macros.nameOf[BiDirectional.type]

  def apply[A, B, C, D](onDownstream: LinkDownstream[A, B])(onUpstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(NAME)(onDownstream)(onUpstream)

  def apply[A : TypeData, B : TypeData, C : TypeData, D : TypeData](name: String)(onDownstream: LinkDownstream[A, B])(onUpstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    ProxySpecification(Metadata(name, implicitly[TypeData[A]], implicitly[TypeData[B]], implicitly[TypeData[C]], implicitly[TypeData[D]]))(onDownstream, onUpstream)
}
