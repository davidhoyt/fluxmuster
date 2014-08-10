package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.Macros

trait BiDirectional extends BiDirectionalLike { self: BiDirectional.Dependencies =>
  def apply[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    run(e)

  def run[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit proofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    upstream(proofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity)))
}

object BiDirectional {
  type Dependencies = Named

  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]) extends Named with BiDirectional {
    type DownstreamIn  = A
    type DownstreamOut = B
    type UpstreamIn    = C
    type UpstreamOut   = D
  }

  def apply[A, B, C, D](downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    apply(Macros.simpleNameOf[BiDirectional.type])(downstream)(upstream)

  def apply[A, B, C, D](name: String)(downstream: Downstream[A, B])(upstream: Upstream[C, D]): BiDi[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream)
}
