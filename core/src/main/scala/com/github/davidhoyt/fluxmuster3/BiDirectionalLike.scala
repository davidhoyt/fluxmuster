package com.github.davidhoyt.fluxmuster3

trait BiDirectionalLike { self: BiDirectionalLike.Dependencies =>
  type DownstreamIn
  type DownstreamOut
  type UpstreamIn
  type UpstreamOut

  val downstream: Downstream[DownstreamIn, DownstreamOut]
  val upstream: Upstream[UpstreamIn, UpstreamOut]

  def asShortString: String =
    null

  val asDefaultString =
    s"$name[${self.downstream.typeIn.toShortString}, ${self.downstream.typeOut.toShortString}, ${self.upstream.typeIn.toShortString}, ${self.upstream.typeOut.toShortString}]"

  val toShortString = {
    val short = asShortString
    if (short ne null)
      short
    else
      asDefaultString
  }

  override def toString =
    toShortString

  override def hashCode: Int =
    downstream.hashCode * 31 + upstream.hashCode

  override def equals(other: Any): Boolean = other match {
    case ref: AnyRef if ref eq this => true
    case ref: BiDirectionalLike if ref.downstream == downstream && ref.upstream == upstream => true
    case _ => false
  }
}

object BiDirectionalLike {
  type Dependencies = Named
}