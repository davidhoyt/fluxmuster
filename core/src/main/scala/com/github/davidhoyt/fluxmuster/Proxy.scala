package com.github.davidhoyt.fluxmuster

import runner._

trait ProxyNeedsProof[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] extends Named {
  import scala.language.higherKinds

  val downstream: Downstream[DownstreamIn, DownstreamOut]
  val upstream: Upstream[UpstreamIn, UpstreamOut]

  implicit def withProof(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn): Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    Proxy(name, downstream, upstream, proofDownstreamCanMapToUpstream)(downstream.typeOut, upstream.typeIn)

  def runProxy[A](in: A)(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, convertIn: A => DownstreamIn): UpstreamOut = {
    val down = downstream.toFunction
    val up = upstream.toFunction
    val composed = up compose proofDownstreamCanMapToUpstream compose down compose convertIn
    composed(in)
  }

  def <~>[A, B, C, D](other: ProxyNeedsProof[A, B, C, D])(implicit connect: DownstreamOut => A, connect2: D => UpstreamIn, connect3: B => C): Proxy[DownstreamIn, B, C, UpstreamOut] =
    combine(other)

  def combine[A, B, C, D](other: ProxyNeedsProof[A, B, C, D])(implicit connect: DownstreamOut => A, connect2: D => UpstreamIn, connect3: B => C): Proxy[DownstreamIn, B, C, UpstreamOut] = {
    val down = downstream andThen other.downstream
    val up = upstream compose other.upstream
    Proxy("<~>", down, up, connect3)(other.downstream.typeOut, other.upstream.typeIn)
  }

  def map[A, B, C, D, T](fn: this.type => T)(implicit evidence: T <:< ProxyNeedsProof[A, B, C, D]): T =
    fn(this)

//  def map[A, B, C, D, T](fn: this.type => T)(implicit evidence: T <:< ProxyNeedsProof[A, B, C, D],  a: DownstreamOut => A, d: D => UpstreamIn): ProxyNeedsProof[DownstreamIn, B, C, UpstreamOut] = {
//    val mapped = evidence(fn(this))
//    if (mapped == this) {
//      mapped.asInstanceOf[ProxyNeedsProof[DownstreamIn, B, C, UpstreamOut]]
//    } else {
//      val mappedDown = mapped.downstream
//      val mappedUp = mapped.upstream
//
//      import downstream.typeIn
//      import downstream.typeOut
//      implicit val v1 = upstream.typeIn
//      implicit val v2 = mappedDown.typeIn
//      implicit val v3 = mappedUp.typeOut
//
//      //Preserve the chain when mapping.
//
//      val newDown = downstream andThen a andThen mappedDown
//      val newUp = upstream compose d compose mappedUp
//
//      Proxy(name, newDown, newUp)
//    }
//  }

  def flatMap[A, B, C, D, T](fn: this.type => T)(implicit evidence: T <:< Run[A, D]): T =
    //Ignore the chain when flatMapping.
    fn(this) //.combineInReverse(withProof)

  def withFilter(fn: this.type => Boolean): this.type = {
    //no-op
    //Still call the function in case it's side-effecting in some way. :`(
    if (fn(this))
      this
    else
      this
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

  override val toString =
    toShortString

  override val hashCode: Int =
    (downstream.hashCode * 31) +
    upstream.hashCode

  override def equals(other: Any): Boolean = other match {
    case ref: AnyRef if ref eq this => true
    case ref: ProxyNeedsProof[_, _, _, _] if ref.downstream == downstream && ref.upstream == upstream => true
    case _ => false
  }
}

trait Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  extends ProxyNeedsProof[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with Chained[DownstreamIn, UpstreamOut]
  with Run[DownstreamIn, UpstreamOut] {
  import Chains._

  import scala.language.higherKinds

  implicit val proofDownstreamCanMapToUpstream: Link[DownstreamOut, UpstreamIn]

  implicit val chain: LinkChain =
    downstream.chain ++ (proofDownstreamCanMapToUpstream +: upstream.chain)

  def run(in: DownstreamIn): UpstreamOut =
    runProxy(in)(proofDownstreamCanMapToUpstream.toFunction, identity)

  lazy val runner =
    toFunction

  implicit lazy val toFunction: DownstreamIn => UpstreamOut =
    run

  implicit lazy val toLink: Link[DownstreamIn, UpstreamOut] =
    Link(name)(toFunction)(downstream.typeIn, upstream.typeOut)

  def |>[A >: DownstreamIn, D <: UpstreamOut, S, F[_]](other: RunnerNeedsProxy[A, D, S, F])(implicit converter: F -> F, typeFofD: TypeTagTree[F[UpstreamOut]]): Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, F, F] =
    lift(other)

  def lift[A >: DownstreamIn, D <: UpstreamOut, S, F[_]](other: RunnerNeedsProxy[A, D, S, F])(implicit converter: F -> F, typeFofD: TypeTagTree[F[UpstreamOut]]): Runner[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, F, F] = {
    val runner = Runner.withUnliftedProxy(other.name, this, newRunnerDataChain(), other.state, other.ops, rewireOnFlatMap = true, mapState = other.mapState)(converter, other.typeState, typeFofD, typeFofD)
    runner
  }

  override val hashCode: Int =
    (downstream.hashCode * 31 * 31) +
    (proofDownstreamCanMapToUpstream.hashCode * 31) +
    upstream.hashCode

  override def equals(other: Any): Boolean = other match {
    case ref: AnyRef if ref eq this => true
    case ref: Proxy[_, _, _, _] if ref.downstream == downstream && ref.upstream == upstream && ref.proofDownstreamCanMapToUpstream == proofDownstreamCanMapToUpstream => true
    case _ => false
  }
}

object Proxy {
  val NAME = Macros.simpleNameOf[Proxy.type]

  private case class BuildWithoutProof[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]) extends ProxyNeedsProof[A, B, C, D]
  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: Link[B, C]) extends Proxy[A, B, C, D] {
    val typeIn = downstream.typeIn
    val typeOut = upstream.typeOut
  }

  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D]): ProxyNeedsProof[A, B, C, D] =
    BuildWithoutProof(name, downstream, upstream)

  def apply[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: B => C)(implicit tB: TypeTagTree[B], tC: TypeTagTree[C]): Proxy[A, B, C, D] =
    Build(name, downstream, upstream, Link(proofDownstreamCanMapToUpstream))
}
