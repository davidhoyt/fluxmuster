package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

object StepDsl {
  type Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] with StepChaining with Named
}

trait StepDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  self: StepDsl.Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =>

  def ~>[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A): Step[DownstreamIn, B, UpstreamIn, UpstreamOut] =
    down(linked)

  def down[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A): Step[DownstreamIn, B, UpstreamIn, UpstreamOut] =
    Step.create(name)(downstream.andThen(linked)(proofDownstreamOutCanRouteToLinkedIn))(upstream)

  def <~[A, B](linked: Link[A, B])(implicit proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): Step[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    up(linked)

  def up[A, B](linked: Link[A, B])(implicit proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): Step[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    Step.create(name)(downstream)(upstream.compose(linked)(proofLinkedOutCanRouteToUpstreamIn))

  def combine[A, B, C, D](other: Step[A, B, C, D])(implicit connect: DownstreamOut => A, connect2: D => UpstreamIn): Step[DownstreamIn, B, C, UpstreamOut] =
    Step.createCombined(chain, other.chain)(downstream andThen other.downstream)(upstream compose other.upstream)
}

object StepRun {
  type Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
}

trait StepRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  self: StepRun.Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =>

  def apply[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    run(e)

  def run[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    upstream(proofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity)))

  /** Generates a [[Link]] instance that when called runs the downstream and then passes it to the upstream. */
  def runLink(implicit proofDownstreamCanRouteToUpstream: DownstreamOut => UpstreamIn): Link[DownstreamIn, UpstreamOut] =
    runLinkEx[DownstreamIn, DownstreamOut, UpstreamIn](proofDownstreamCanRouteToUpstream, identity, downstream.typeIn)

  /** Generates a [[Link]] instance that when called runs the downstream and then passes it to the upstream. */
  def runLinkEx[E, F >: DownstreamOut, G <: UpstreamIn](implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn, tE: TypeTagTree[E]): Link[E, UpstreamOut] =
    Link((e: E) => upstream(proofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity))))(tE, upstream.typeOut)
}

trait Step[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  extends StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with StepDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with StepRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with StepChaining {
  self: Named =>

}

object EmptyStep {
  private case class Build(name: String) extends EmptyStep {
    override def toString =
      s"${Macros.simpleNameOf[EmptyStep.type]}($name)"
  }

  def apply(name: String): EmptyStep =
    Build(name)
}

trait EmptyStep extends Named {
  def ~>[A, B](linked: Link[A, B]): Step[A, B, B, B] =
    down(linked)

  def down[A, B](linked: Link[A, B]): Step[A, B, B, B] =
    Step.create[A, B, B, B](name)(linked)(Link.identity[B](linked.typeOut))

  def <~[A, B](linked: Link[A, B]): Step[A, A, A, B] =
    up(linked)

  def up[A, B](linked: Link[A, B]): Step[A, A, A, B] =
    Step.create[A, A, A, B](name)(Link.identity[A](linked.typeIn))(linked)
}

object Step {
  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], mine: ChainBiDi, other: ChainBiDi)(val chaining: (ChainableBiDi, ChainBiDi, ChainBiDi) => ChainBiDi) extends Step[A, B, C, D] with Named {
    lazy val chain =
      chainTogether(this, mine, other)

    def chainTogether(instance: ChainableBiDi, mine: ChainBiDi, other: ChainBiDi): ChainBiDi =
      chaining(instance, mine, other)

    override def toString =
      s"${Macros.simpleNameOf[Step.type]}($name)[${downstream.typeIn.toShortString}, ${downstream.typeOut.toShortString}, ${upstream.typeIn.toShortString}, ${upstream.typeOut.toShortString}]"
  }

  def ~>[A, B](linked: Link[A, B]): Step[A, B, B, B] =
    create[A, B, B, B](linked)(Link.identity[B](linked.typeOut))

  def <~[A, B](linked: Link[A, B]): Step[A, A, A, B] =
    create[A, A, A, B](Link.identity[A](linked.typeIn))(linked)

  def create[A, B, C, D](downstream: Downstream[A, B])(upstream: Upstream[C, D]): Step[A, B, C, D] =
    create(Macros.simpleNameOf[Step.type])(downstream)(upstream)

  def create[A, B, C, D](name: String)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): Step[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream, EmptyChainBiDi, EmptyChainBiDi)(BiDirectionalProvidedChain.apply)

  def createCombined[A, B, C, D](mine: ChainBiDi, other: ChainBiDi)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): Step[A, B, C, D] =
    createCombined(Macros.simpleNameOf[Step.type])(mine, other)(downstream)(upstream)

  def createCombined[A, B, C, D](name: String)(mine: ChainBiDi, other: ChainBiDi)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): Step[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream, mine, other)(BiDirectionalCombinedChain.apply)

  def apply(name: String): EmptyStep =
    EmptyStep(name)
}
