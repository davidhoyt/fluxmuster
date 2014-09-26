package com.github.davidhoyt.fluxmuster

sealed trait Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
extends Named { self => 
  import scala.language.higherKinds

  val downstream: Downstream[DownstreamIn, DownstreamOut]
  val upstream: Upstream[UpstreamIn, UpstreamOut]

  implicit def linked(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn): LinkedProxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    Proxy.linked(name, downstream, upstream, proofDownstreamCanMapToUpstream)

  def runProxy[A](in: A)(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, convertIn: A => DownstreamIn): UpstreamOut = {
    val down = downstream.toFunction
    val up = upstream.toFunction
    val composed = up compose proofDownstreamCanMapToUpstream compose down compose convertIn
    composed(in)
  }

  def <~>[A, B, C, D](other: Proxy[A, B, C, D])(implicit a: DownstreamOut => A, d: D => UpstreamIn): Proxy[DownstreamIn, B, C, UpstreamOut] =
    combine(other)

  /**
   * Links this [[Proxy]] instance with another. The downstream output type must match `other`'s
   * downstream input type. `other`'s upstream output type must match this [[Proxy]] instance's
   * upstream input type. It forms a bidirectional pipe between the instances where data
   * automatically flows downstream from one to the other and then in reverse order flows
   * back upstream.
   *
   * This method produces an immutable combined [[LinkedProxy]].
   *
   * @param other The [[Proxy]] instance with which this instance will be combined.
   * @param a
   * @param d
   * @tparam A
   * @tparam B
   * @tparam C
   * @tparam D
   * @return
   */
  def combine[A, B, C, D](other: Proxy[A, B, C, D])(implicit a: DownstreamOut => A, d: D => UpstreamIn): Proxy[DownstreamIn, B, C, UpstreamOut] = {
    val down = downstream andThen other.downstream
    val up = upstream compose other.upstream
    Proxy("<~>", down, up)
  }

  def |>[A >: DownstreamIn, D <: UpstreamOut, S, F[_]](other: PartialLift[A, D, S, F])(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, converter: F -> F, typeFofD: TypeTagTree[F[UpstreamOut]]): Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, F, F] =
    lift(other)

  /**
   * Provided as a courtesy for more meaningful error messages when proof (an implicitly available
   * function that can map the downstream out to the upstream in) is '''not''' available.
   *
   * Please see [[LinkedProxy#lift]] for details.
   *
   * @param other
   * @param proofDownstreamCanMapToUpstream
   * @param converter
   * @param typeFofD
   * @tparam A
   * @tparam D
   * @tparam S
   * @tparam F
   * @return
   */
  def lift[A >: DownstreamIn, D <: UpstreamOut, S, F[_]](other: PartialLift[A, D, S, F])(implicit proofDownstreamCanMapToUpstream: DownstreamOut => UpstreamIn, converter: F -> F, typeFofD: TypeTagTree[F[UpstreamOut]]): Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, F, F] =
    linked(proofDownstreamCanMapToUpstream) lift other

  def map[A, B, C, D, T](fn: this.type => T)(implicit evidence: T <:< Proxy[A, B, C, D]): T =
    fn(this)

  /**
   * Calls the provided function with this [[Proxy]] instance allowing anything to be returned.
   * It's advisable that an instance of [[Run]] be the return value. Valid [[Run]] types include
   * [[Link]], [[LinkedProxy]], and [[Lift]].
   *
   * This is primarily used for proxy construction in a for comprehension.
   *
   * @param fn A function that returns a [[Run]] instance.
   * @tparam T The actual return type of `fn`.
   * @return The result of the calling `fn` with this [[Proxy]] instance.
   */
  def flatMap[T](fn: this.type => T): T =
    fn(this)

  /**
   * Proxies are not actually monadic in that they are not contextual. Data either flows
   * through the entire composed proxy or it does not. Filters would prevent data from
   * flowing into the next composed proxy and therefore may cause it to not flow back
   * upstream.
   *
   * Due to the nature of proxies, filters do nothing except prevent compiler warnings.
   * It would be better if it could be omitted entirely.
   *
   * @param fn A predicate that will be given an instance of this [[Proxy]] but will
   *           otherwise do nothing. Any side effects would be exhibited at __proxy
   *           construction time not when evaluating data through the proxy__.
   * @return This same [[Proxy]] instance.
   */
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
    case ref: Proxy[_, _, _, _] if ref.downstream == downstream && ref.upstream == upstream => true
    case _ => false
  }
}

/**
 * Describes a [[Proxy]] that has its downstream and its upstream linked together.
 * When the downstream and upstream are properly linked, runs can properly flow
 * through the downstream, through the link, and then back upstream and out.
 * 
 * @tparam DownstreamIn
 * @tparam DownstreamOut
 * @tparam UpstreamIn
 * @tparam UpstreamOut
 */
sealed trait LinkedProxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  extends Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with Chain[DownstreamIn, UpstreamOut]
  with Run[DownstreamIn, UpstreamOut] {
  import Chains._

  import scala.language.higherKinds

  implicit val linkDownstreamToUpstream: Link[DownstreamOut, UpstreamIn]

  implicit val chain: LinkChain =
    downstream.chain ++ (linkDownstreamToUpstream +: upstream.chain)

  def run(in: DownstreamIn): UpstreamOut =
    runProxy(in)(linkDownstreamToUpstream.toFunction, identity)

  lazy val runner =
    toFunction

  implicit val toFunction: DownstreamIn => UpstreamOut =
    run

  implicit val toLink: Link[DownstreamIn, UpstreamOut] =
    Link(name)(toFunction)(downstream.typeIn, upstream.typeOut)

  def |>[A >: DownstreamIn, D <: UpstreamOut, S, F[_]](other: PartialLift[A, D, S, F])(implicit converter: F -> F, typeFofD: TypeTagTree[F[UpstreamOut]]): Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, F, F] =
    lift(other)

  def lift[A >: DownstreamIn, D <: UpstreamOut, S, F[_]](other: PartialLift[A, D, S, F])(implicit converter: F -> F, typeFofD: TypeTagTree[F[UpstreamOut]]): Lift[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, S, F, F] = {
    val runner = Lift.proxy(other.name, this, newLiftChain(), other.state, other.ops, rewireOnFlatMap = true)(converter, other.typeState, typeFofD, typeFofD)
    runner
  }

  override val hashCode: Int =
    (downstream.hashCode * 31 * 31) +
    (linkDownstreamToUpstream.hashCode * 31) +
    upstream.hashCode

  override def equals(other: Any): Boolean = other match {
    case ref: AnyRef if ref eq this => true
    case ref: LinkedProxy[_, _, _, _] if ref.downstream == downstream && ref.upstream == upstream && ref.linkDownstreamToUpstream == linkDownstreamToUpstream => true
    case _ => false
  }
}

object LinkedProxy

object Proxy {
  val defaultName =
    Macros.simpleNameOf[Proxy.type]

  private case class BuildWithoutProof[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D])
    extends Proxy[A, B, C, D]

  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], linkDownstreamToUpstream: Link[B, C])
    extends LinkedProxy[A, B, C, D] {
    val typeIn  = downstream.typeIn
    val typeOut = upstream.typeOut
  }

  def apply[A, B, C, D](name: String = defaultName, downstream: Downstream[A, B], upstream: Upstream[C, D]): Proxy[A, B, C, D] =
    BuildWithoutProof(name, downstream, upstream)

  def linked[A, B, C, D](downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: B => C): LinkedProxy[A, B, C, D] =
    linked(defaultName, downstream, upstream, Link(proofDownstreamCanMapToUpstream)(downstream.typeOut, upstream.typeIn))

  def linked[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: B => C): LinkedProxy[A, B, C, D] =
    linked(name, downstream, upstream, Link(proofDownstreamCanMapToUpstream)(downstream.typeOut, upstream.typeIn))

  def linked[A, B, C, D](name: String = defaultName, downstream: Downstream[A, B], upstream: Upstream[C, D], proofDownstreamCanMapToUpstream: Link[B, C]): LinkedProxy[A, B, C, D] =
    Build(name, downstream, upstream, proofDownstreamCanMapToUpstream)
}
