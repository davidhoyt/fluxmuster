package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

object BiDirectionalDsl {
  type Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    BiDirectionalLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] with BiDirectionalChaining with Named
}

trait BiDirectionalDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  self: BiDirectionalDsl.Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =>

  def ~>[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A): BiDi[DownstreamIn, B, UpstreamIn, UpstreamOut] =
    down(linked)

  def down[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A): BiDi[DownstreamIn, B, UpstreamIn, UpstreamOut] =
    BiDirectional.create(name)(downstream.andThen(linked)(proofDownstreamOutCanRouteToLinkedIn))(upstream)

  def <~[A, B](linked: Link[A, B])(implicit proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): BiDi[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    up(linked)

  def up[A, B](linked: Link[A, B])(implicit proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): BiDi[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    BiDirectional.create(name)(downstream)(upstream.compose(linked)(proofLinkedOutCanRouteToUpstreamIn))

//  protected def combineChains(mine: ChainBiDi, other: ChainBiDi): ChainBiDi =
//    (mine ++ other).foldLeft(EmptyChainBiDi) {
//      case (seq, p) if p.chain.nonEmpty =>
//        seq :+ p.chain.head
//      case (seq, _) =>
//        seq
//    }

  def combine[A, B, C, D](other: BiDi[A, B, C, D])(implicit connect: DownstreamOut => A, connect2: D => UpstreamIn): BiDi[DownstreamIn, B, C, UpstreamOut] = {
    val down = downstream andThen other.downstream
    val up = upstream compose other.upstream
    val foo = BiDirectional.createCombined[DownstreamIn, B, C, UpstreamOut](chain, other.chain)(down)(up)
//    val combinedChain =
    foo
  }
}

object BiDirectionalRun {
  type Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    BiDirectionalLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
}

trait BiDirectionalRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  self: BiDirectionalRun.Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =>

  def apply[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    run(e)

  def run[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    upstream(proofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity)))
}

trait BiDirectional[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  extends BiDirectionalLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with BiDirectionalDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with BiDirectionalRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with BiDirectionalChaining {
  self: Named =>

}

object EmptyBiDirectional {
  private case class Build(name: String) extends EmptyBiDirectional {
    override def toString =
      s"${Macros.simpleNameOf[EmptyBiDirectional.type]}($name)"
  }

  def apply(name: String): EmptyBiDirectional =
    Build(name)
}

trait EmptyBiDirectional extends Named {
  def ~>[A, B](linked: Link[A, B]): BiDi[A, B, B, B] =
    down(linked)

  def down[A, B](linked: Link[A, B]): BiDi[A, B, B, B] =
    BiDirectional.create[A, B, B, B](name)(linked)(Link.identity[B](linked.typeOut))

  def <~[A, B](linked: Link[A, B]): BiDi[A, A, A, B] =
    up(linked)

  def up[A, B](linked: Link[A, B]): BiDi[A, A, A, B] =
    BiDirectional.create[A, A, A, B](name)(Link.identity[A](linked.typeIn))(linked)
}

object BiDirectional {
  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], mine: ChainBiDi, other: ChainBiDi)(val chaining: (ChainableBiDi, ChainBiDi, ChainBiDi) => ChainBiDi) extends BiDirectional[A, B, C, D] with Named {
    lazy val chain =
      chainTogether(this, mine, other)

    def chainTogether(instance: ChainableBiDi, mine: ChainBiDi, other: ChainBiDi): ChainBiDi =
      chaining(instance, mine, other)

    override def toString =
      s"${Macros.simpleNameOf[BiDirectional.type]}($name)[${downstream.typeIn.toShortString}, ${downstream.typeOut.toShortString}, ${upstream.typeIn.toShortString}, ${upstream.typeOut.toShortString}]"
  }

  def ~>[A, B](linked: Link[A, B]): BiDi[A, B, B, B] =
    create[A, B, B, B](linked)(Link.identity[B](linked.typeOut.asInstanceOf[TypeTagTree[B]]))

  def <~[A, B](linked: Link[A, B]): BiDi[A, A, A, B] =
    create[A, A, A, B](Link.identity[A](linked.typeIn.asInstanceOf[TypeTagTree[A]]))(linked)

  def create[A, B, C, D](downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    create(Macros.simpleNameOf[BiDirectional.type])(downstream)(upstream)

  def create[A, B, C, D](name: String)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream, EmptyChainBiDi, EmptyChainBiDi)(BiDirectionalProvidedChain.apply)

  def createCombined[A, B, C, D](mine: ChainBiDi, other: ChainBiDi)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    createCombined(Macros.simpleNameOf[BiDirectional.type])(mine, other)(downstream)(upstream)

  def createCombined[A, B, C, D](name: String)(mine: ChainBiDi, other: ChainBiDi)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream, mine, other)(BiDirectionalCombinedChain.apply)

  def apply(name: String): EmptyBiDirectional =
    EmptyBiDirectional(name)
}
