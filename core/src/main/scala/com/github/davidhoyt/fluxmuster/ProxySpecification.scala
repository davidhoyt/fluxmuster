package com.github.davidhoyt.fluxmuster

import scala.collection._

trait ProxySpecification[-TAcceptDownstream, +TMappedDownstream, -TAcceptUpstream, +TMappedUpstream] {
  val metadata: ConnectedMetadata
  val downstream: LinkDownstream[TAcceptDownstream, TMappedDownstream]
  val upstream: LinkUpstream[TAcceptUpstream, TMappedUpstream]
  val connections: immutable.Seq[ProxySpecification[_, _, _, _]]

  override def toString = s"ProxySpecification(${metadata.mkString(" <~> ")})"
}

object ProxySpecification {
  import scala.reflect.runtime.universe._
  import scala.language.implicitConversions

  def apply[A, B, C, D](downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxySpecification[A, B, C, D] =
    apply(immutable.Seq(Metadata(s"<unknown>", tA, tB, tC, tD)), downstream, upstream)

  def apply[A, B, C, D](metadata: Metadata)(downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(immutable.Seq(metadata), downstream, upstream)

  def apply[A, B, C, D](specMetadata: ConnectedMetadata, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(specMetadata, downstream, upstream, immutable.Seq())

  def apply[A, B, C, D](specMetadata: ConnectedMetadata, specDownstream: LinkDownstream[A, B], specUpstream: LinkUpstream[C, D], specConnections: immutable.Seq[ProxySpecification[_, _, _, _]]): ProxySpecification[A, B, C, D] = {
    new ProxySpecification[A, B, C, D] {
      val metadata   = specMetadata
      val downstream = specDownstream
      val upstream   = specUpstream
      val connections = {
        if (specConnections.nonEmpty)
          specConnections
        else
          immutable.Seq[ProxySpecification[_, _, _, _]](this)
      }
    }
  }

  def unapply[A, B, C, D](spec: ProxySpecification[A, B, C, D]): Option[(ConnectedMetadata, LinkDownstream[A, B], LinkUpstream[C, D], immutable.Seq[ProxySpecification[_, _, _, _]])] =
    PartialFunction.condOpt(spec) {
      case s => (s.metadata, s.downstream, s.upstream, s.connections)
    }

  def combine[A, B /*: TypeTag*/, C /*: TypeTag*/, E, F, G](p1: ProxySpecification[A, B, G, E], p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] = {
    //val bTag = implicitly[TypeTag[B]]
    //val gTag = implicitly[TypeTag[G]]
    val ProxySpecification(metadata1, downstream1, upstream1, connections1) = p1
    val ProxySpecification(metadata2, downstream2, upstream2, connections2) = p2

    //val check: LinkDownstream[B, B] = {
    //  case next @ ShortCircuit(_) if gTag.tpe <:< bTag.tpe => upstream1()
    //  case next => next
    //}

    val downstream: LinkDownstream[A, C] = downstream1 andThen downstream2
    val upstream: LinkUpstream[F, E] = upstream1 compose upstream2
    val connections: immutable.Seq[ProxySpecification[_, _, _, _]] =
      (connections1 ++ connections2).foldLeft(immutable.Seq[ProxySpecification[_, _, _, _]]()) {
        case (seq, ProxySpecification(_, _, _, Seq(conn))) =>
          seq :+ conn
        case (seq, _) =>
          seq
      }
    ProxySpecification(metadata1 ++ metadata2, downstream, upstream, connections)
  }

  def run[A, B, C](p: ProxySpecification[A, B, B, C])(value: A): C = {
    val fromDownstream = p.downstream(value)
    val result = p.upstream(fromDownstream)
    result
  }
}