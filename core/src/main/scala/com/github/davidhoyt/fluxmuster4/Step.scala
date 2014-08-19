package com.github.davidhoyt.fluxmuster4

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

trait StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] extends Named {
  val downstream: Downstream[DownstreamIn, DownstreamOut]
  val upstream: Upstream[UpstreamIn, UpstreamOut]

  def withProof(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, tOut: TypeTagTree[DownstreamOut], tIn: TypeTagTree[UpstreamIn]): Step[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    Step(name, downstream, upstream, proofDownstreamCanMapToUpstream)(tOut, tIn)

  def run[A](in: A)(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, convertIn: A => DownstreamIn): UpstreamOut = {
    val down = downstream.toFunction
    val up = upstream.toFunction
    val composed = up compose proofDownstreamCanMapToUpstream compose down compose convertIn
    composed(in)
  }

  def combine[A, B, C, D](other: StepLike[A, B, C, D])(implicit connect: DownstreamOut => A, connect2: D => UpstreamIn, connect3: B => C): Step[DownstreamIn, B, C, UpstreamOut] = {
    val down = downstream andThen other.downstream
    val up = upstream compose other.upstream
    Step("<~>", down, up, connect3)(other.downstream.typeOut, other.upstream.typeIn)
  }

  def asShortString: String =
    null

  val asDefaultString = {
    val up = Option(upstream)
    val down = Option(downstream)

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
    case ref: StepLike[_, _, _, _] if ref.downstream == downstream && ref.upstream == upstream => true
    case _ => false
  }
}

trait Step[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  extends StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with Chained {

  implicit val proofDownstreamCanMapToUpstream: Link[DownstreamOut, UpstreamIn]

  implicit val chain: ChainLink =
    downstream.chain ++ (proofDownstreamCanMapToUpstream +: upstream.chain)

  def runChain(in: DownstreamIn): UpstreamOut = {
    val ran = chain.foldLeft(in: Any) {
      case (soFar, next) =>
//        next.typeIn.symbol.asInstanceOf
        next.runAny(soFar)
    }
    ran.asInstanceOf[UpstreamOut]
  }

}

object Step {
  val NAME = Macros.simpleNameOf[Step.type]

  private case class BuildWithoutProof[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]) extends StepLike[A, B, C, D]
  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: Link[B, C]) extends Step[A, B, C, D]

  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]): StepLike[A, B, C, D] =
    BuildWithoutProof(name, downstream, upstream)

  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: B => C)(implicit tB: TypeTagTree[B], tC: TypeTagTree[C]): Step[A, B, C, D] =
    Build(name, downstream, upstream, Link(proofDownstreamCanMapToUpstream))
}
