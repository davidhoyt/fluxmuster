package com.github.davidhoyt.fluxmuster4

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

trait StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] extends Named {
  val downstream: Downstream[DownstreamIn, DownstreamOut]
  val upstream: Upstream[UpstreamIn, UpstreamOut]

  def withProof(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, tOut: TypeTagTree[DownstreamOut], tIn: TypeTagTree[UpstreamIn]): Step[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    Step(name, downstream, upstream, proofDownstreamCanMapToUpstream)(tOut, tIn)

  def runStep[A](in: A)(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, convertIn: A => DownstreamIn): UpstreamOut = {
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
  with Chained[DownstreamIn, UpstreamOut] {

  implicit val proofDownstreamCanMapToUpstream: Link[DownstreamOut, UpstreamIn]

  implicit val chain: ChainLink =
    downstream.chain ++ (proofDownstreamCanMapToUpstream +: upstream.chain)

  def run(in: DownstreamIn): UpstreamOut =
    runStep(in)(proofDownstreamCanMapToUpstream.toFunction, identity)

  lazy val runner =
    toFunction

  implicit lazy val toFunction: DownstreamIn => UpstreamOut =
    run

  implicit lazy val toLink: Link[DownstreamIn, UpstreamOut] =
    Link(name)(toFunction)(downstream.typeIn, upstream.typeOut)

  def map[A, B, C, D](fn: ((Downstream[DownstreamIn, DownstreamOut], Upstream[UpstreamIn, UpstreamOut])) => (Downstream[A, B], Upstream[C, D]))(implicit connect: DownstreamOut => A, connect2: D => UpstreamIn, connect3: B => C, tB: TypeTagTree[B], tC: TypeTagTree[C]): Step[DownstreamIn, B, C, UpstreamOut] = {
    val (otherDown, otherUp) = fn((downstream, upstream))
    val down = downstream andThen otherDown
    val up = upstream compose otherUp
    Step("<~>", down, up, connect3)
  }

  def filter(fn: ((Downstream[DownstreamIn, DownstreamOut], Upstream[UpstreamIn, UpstreamOut])) => Boolean): Step[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] = {
    //no-op
    //Still call the function in case it's side-effecting in some way. :`(
    if (fn((downstream, upstream)))
      this
    else
      this
  }

}

object Step {
  val NAME = Macros.simpleNameOf[Step.type]

  private case class BuildWithoutProof[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]) extends StepLike[A, B, C, D]
  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: Link[B, C]) extends Step[A, B, C, D] {
    val typeIn = downstream.typeIn
    val typeOut = upstream.typeOut
  }

  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]): StepLike[A, B, C, D] =
    BuildWithoutProof(name, downstream, upstream)

  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: B => C)(implicit tB: TypeTagTree[B], tC: TypeTagTree[C]): Step[A, B, C, D] =
    Build(name, downstream, upstream, Link(proofDownstreamCanMapToUpstream))
}
