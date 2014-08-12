package com.github.davidhoyt.fluxmuster3

object BiDirectionalLike {
  type Dependencies = Named
}

trait BiDirectionalLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] { self: BiDirectionalLike.Dependencies =>

  val downstream: Downstream[DownstreamIn, DownstreamOut]
  val upstream: Upstream[UpstreamIn, UpstreamOut]

  def asShortString: String =
    null

  val asDefaultString = {
    val up = Option(self.upstream)
    val down = Option(self.downstream)

    val upIn = up map (_.typeIn.toShortString) getOrElse "<unknown>"
    val upOut = up map (_.typeOut.toShortString) getOrElse "<unknown>"

    val downIn = down map (_.typeIn.toShortString) getOrElse "<unknown>"
    val downOut = down map (_.typeOut.toShortString) getOrElse "<unknown>"

    s"$name[$downIn, $downOut, $upIn, $upOut]"
  }

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
    case ref: BiDirectionalLike[_, _, _, _] if ref.downstream == downstream && ref.upstream == upstream => true
    case _ => false
  }
}
