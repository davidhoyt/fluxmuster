package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

object BiDirectionalDsl {
  type Dependencies = BiDirectionalLike with Named
}

trait BiDirectionalDsl { self: BiDirectionalDsl.Dependencies =>
  def ~>[A, B](linked: Linked[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A): BiDi[DownstreamIn, B, UpstreamIn, UpstreamOut] =
    down(linked)

  def down[A, B](linked: Linked[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A): BiDi[DownstreamIn, B, UpstreamIn, UpstreamOut] =
    BiDirectional.create(name)(downstream.andThen(linked)(proofDownstreamOutCanRouteToLinkedIn))(upstream)

  def <~[A, B](linked: Linked[A, B])(implicit proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): BiDi[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    up(linked)

  def up[A, B](linked: Linked[A, B])(implicit proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): BiDi[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    BiDirectional.create(name)(downstream)(upstream.compose(linked)(proofLinkedOutCanRouteToUpstreamIn))

  def combine[A, B, C, D](other: BiDi[A, B, C, D]): BiDi[DownstreamIn, B, C, UpstreamOut] =
    ???
}

object BiDirectionalRun {
  type Dependencies = BiDirectionalLike
}

trait BiDirectionalRun { self: BiDirectionalRun.Dependencies =>
  def apply[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    run(e)

  def run[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    upstream(proofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity)))
}

trait BiDirectional extends BiDirectionalLike with BiDirectionalDsl with BiDirectionalRun with Named

object EmptyBiDirectional {
  private case class Build(name: String) extends EmptyBiDirectional {
    override def toString =
      s"${Macros.simpleNameOf[EmptyBiDirectional.type]}($name)"
  }

  def apply(name: String): EmptyBiDirectional =
    Build(name)
}

trait EmptyBiDirectional extends Named {
  def ~>[A, B](linked: Linked[A, B]): BiDi[A, B, B, B] =
    down(linked)

  def down[A, B](linked: Linked[A, B]): BiDi[A, B, B, B] =
    BiDirectional.create[A, B, B, B](name)(linked)(Link.identity[B](linked.typeOut.asInstanceOf[TypeTagTree[B]]))

  def <~[A, B](linked: Linked[A, B]): BiDi[A, A, A, B] =
    up(linked)

  def up[A, B](linked: Linked[A, B]): BiDi[A, A, A, B] =
    BiDirectional.create[A, A, A, B](name)(Link.identity[A](linked.typeIn.asInstanceOf[TypeTagTree[A]]))(linked)
}

object BiDirectional {
  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]) extends Named with BiDirectional {
    type DownstreamIn  = A
    type DownstreamOut = B
    type UpstreamIn    = C
    type UpstreamOut   = D

    override def toString =
      s"${Macros.simpleNameOf[BiDirectional.type]}($name)[${downstream.typeIn.toShortString}, ${downstream.typeOut.toShortString}, ${upstream.typeIn.toShortString}, ${upstream.typeOut.toShortString}]"
  }

  def ~>[A, B](linked: Linked[A, B]): BiDi[A, B, B, B] =
    create[A, B, B, B](linked)(Link.identity[B](linked.typeOut.asInstanceOf[TypeTagTree[B]]))

  def <~[A, B](linked: Linked[A, B]): BiDi[A, A, A, B] =
    create[A, A, A, B](Link.identity[A](linked.typeIn.asInstanceOf[TypeTagTree[A]]))(linked)

  def create[A, B, C, D](downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    create(Macros.simpleNameOf[BiDirectional.type])(downstream)(upstream)

  def create[A, B, C, D](name: String)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream)

  def apply(name: String): EmptyBiDirectional =
    EmptyBiDirectional(name)
}
