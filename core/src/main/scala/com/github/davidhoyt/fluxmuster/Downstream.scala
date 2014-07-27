package com.github.davidhoyt.fluxmuster



object Downstream {
  val NAME = Macros.simpleNameOf[Downstream.type]

  def apply[A, B, C](onDownstream: LinkDownstream[A, B])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxyStep[A, B, C, C] =
    apply(NAME)(onDownstream)(tA, tB, tC)

  def apply[A, B, C](name: String)(onDownstream: LinkDownstream[A, B])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxyStep[A, B, C, C] =
    ProxyStep(Metadata(name, tA, tB, tC, tC, s"$name[${tA.toShortString}, ${tB.toShortString}, ${tC.toShortString}]"))(onDownstream, identity)

//  implicit class Function1Enhancements[A, B](val fn: A => B)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]) {
////    def <~[E, G, H](step: ProxyStep[E, A, G, H])(implicit tH: TypeTagTree[H]): ProxyStep[E, B, G, H] = {
////      val d = Downstream[A, B, H](onDownstream)(tA, tB, tH)
////      d connect step
////    }
//
//    def <~[C, D](upstreamNext: LinkUpstream[C, A])(implicit tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[D, D, C, B] =
//      Upstream[D, A, B](fn)(tD, tA, tB) connect Upstream[D, C, A](upstreamNext)(tD, tC, tA)
//
//    def ~>[C, D](next: LinkDownstream[B, C])(implicit tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, C, D, D] =
//      Downstream[A, B, D](fn)(tA, tB, tD) connect Downstream[B, C, D](next)(tB, tC, tD)
//  }
//
//  implicit class ProxyStepEnhancements[A, B, C, D](val step: ProxyStep[A, B, C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]) {
//    def <~[E](onUpstream: LinkUpstream[E, C])(implicit tE: TypeTagTree[E]): ProxyStep[A, B, E, D] =
//      step connect Upstream[B, E, C](onUpstream)(tB, tE, tC)
//
//    def ~>[E](onDownstream: LinkDownstream[B, E])(implicit tE: TypeTagTree[E]): ProxyStep[A, E, C, D] =
//      step connect Downstream[B, E, C](onDownstream)(tB, tE, tC)
//  }
}
