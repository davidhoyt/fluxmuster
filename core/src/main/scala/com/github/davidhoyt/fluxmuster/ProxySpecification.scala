package com.github.davidhoyt.fluxmuster

import scala.collection._

trait ProxySpecification[-TAcceptDownstream, +TMappedDownstream, -TAcceptUpstream, +TMappedUpstream] {
  val metadata: ConnectedMetadata
  val downstream: LinkDownstream[TAcceptDownstream, TMappedDownstream]
  val upstream: LinkUpstream[TAcceptUpstream, TMappedUpstream]

  override def toString = s"ProxySpecification(${metadata.mkString(" <~> ")})"
}

object ProxySpecification {
  import scala.reflect.runtime.universe._
  import scala.language.implicitConversions

  def nameFor[A : TypeTag]: String = {
    val tag = implicitly[TypeTag[A]]
    tag.tpe.termSymbol.name.decoded
  }

  def apply[A, B, C, D](downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(immutable.Seq(s"<unknown>"), downstream, upstream)

  def apply[A, B, C, D](metadata: Metadata)(downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(immutable.Seq(metadata), downstream, upstream)

  private[fluxmuster] def apply[A, B, C, D](specMetadata: ConnectedMetadata, specDownstream: LinkDownstream[A, B], specUpstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    new ProxySpecification[A, B, C, D] {
      val metadata   = specMetadata
      val downstream = specDownstream
      val upstream   = specUpstream
    }

  def unapply[A, B, C, D](spec: ProxySpecification[A, B, C, D]): Option[(ConnectedMetadata, LinkDownstream[A, B], LinkUpstream[C, D])] =
    if (spec ne null)
      Some(spec.metadata, spec.downstream, spec.upstream)
    else
      None

  def combine[A, B /*: TypeTag*/, C /*: TypeTag*/, E, F, G](p1: ProxySpecification[A, B, G, E], p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] = {
    //val bTag = implicitly[TypeTag[B]]
    //val gTag = implicitly[TypeTag[G]]
    val ProxySpecification(metadata1, downstream1, upstream1) = p1
    val ProxySpecification(metadata2, downstream2, upstream2) = p2

    //val check: LinkDownstream[B, B] = {
    //  case next @ ShortCircuit(_) if gTag.tpe <:< bTag.tpe => upstream1()
    //  case next => next
    //}

    val downstream: LinkDownstream[A, C] = downstream1 andThen downstream2
    val upstream: LinkUpstream[F, E] = upstream1 compose upstream2
    ProxySpecification(metadata1 ++ metadata2, downstream, upstream)
  }

  def run[A, B, C](p: ProxySpecification[A, B, B, C])(value: A): C = {
    val fromDownstream = p.downstream(value)
    val result = p.upstream(fromDownstream)
    result
  }

  implicit def function2ProxySpecification[A, B, C, D](fn: A => B): ProxySpecification[A, B, B, B] =
    ProxySpecification(fn, identity)

  implicit class ProxySpecificationEnhancements[A, B, G, E](val p1: ProxySpecification[A, B, G, E]) extends AnyVal {
    def <~>[C, F](p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] =
      connect(p2)
    def connect[C, F](p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] =
      combine(p1, p2)

    def <~>[C, F](link: LinkDownstream[B, C]): ProxySpecification[A, C, G, E] =
      connect(link)
    def connect[C, F](link: LinkDownstream[B, C]): ProxySpecification[A, C, G, E] = {
      val spec = ProxySpecification(link, identity[G])
      val combined = combine(p1, spec)
      combined
    }
  }
}