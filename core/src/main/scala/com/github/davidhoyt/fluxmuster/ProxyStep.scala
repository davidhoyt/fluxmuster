package com.github.davidhoyt.fluxmuster

import scala.collection._

trait ProxyStep[-TAcceptDownstream, +TMappedDownstream, -TAcceptUpstream, +TMappedUpstream] {
  val metadata: ConnectedMetadata
  val downstream: LinkDownstream[TAcceptDownstream, TMappedDownstream]
  val upstream: LinkUpstream[TAcceptUpstream, TMappedUpstream]
  val connections: immutable.Seq[ProxyStep[_, _, _, _]]

  override def toString = s"ProxyStep(${metadata.mkString(" <~> ")})"
}

object ProxyStep {
  import scala.reflect.runtime.universe._
  import scala.language.implicitConversions

  def apply[A, B, C, D](downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, B, C, D] =
    apply(immutable.Seq(Metadata(s"<unknown>", tA, tB, tC, tD)), downstream, upstream)

  def apply[A, B, C, D](metadata: Metadata)(downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxyStep[A, B, C, D] =
    apply(immutable.Seq(metadata), downstream, upstream)

  def apply[A, B, C, D](stepMetadata: ConnectedMetadata, downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxyStep[A, B, C, D] =
    apply(stepMetadata, downstream, upstream, immutable.Seq())

  def apply[A, B, C, D](stepMetadata: ConnectedMetadata, stepDownstream: LinkDownstream[A, B], stepUpstream: LinkUpstream[C, D], stepConnections: immutable.Seq[ProxyStep[_, _, _, _]]): ProxyStep[A, B, C, D] = {
    new ProxyStep[A, B, C, D] {
      val metadata   = stepMetadata
      val downstream = stepDownstream
      val upstream   = stepUpstream
      val connections = {
        if (stepConnections.nonEmpty)
          stepConnections
        else
          immutable.Seq[ProxyStep[_, _, _, _]](this)
      }
    }
  }

  def unapply[A, B, C, D](step: ProxyStep[A, B, C, D]): Option[(ConnectedMetadata, LinkDownstream[A, B], LinkUpstream[C, D], immutable.Seq[ProxyStep[_, _, _, _]])] =
    PartialFunction.condOpt(step) {
      case s => (s.metadata, s.downstream, s.upstream, s.connections)
    }

  def combine[A, B /*: TypeTag*/, C /*: TypeTag*/, E, F, G](p1: ProxyStep[A, B, G, E], p2: ProxyStep[B, C, F, G]): ProxyStep[A, C, F, E] = {
    //val bTag = implicitly[TypeTag[B]]
    //val gTag = implicitly[TypeTag[G]]
    val ProxyStep(metadata1, downstream1, upstream1, connections1) = p1
    val ProxyStep(metadata2, downstream2, upstream2, connections2) = p2

    //val check: LinkDownstream[B, B] = {
    //  case next @ ShortCircuit(_) if gTag.tpe <:< bTag.tpe => upstream1()
    //  case next => next
    //}

    val downstream: LinkDownstream[A, C] = downstream1 andThen downstream2
    val upstream: LinkUpstream[F, E] = upstream1 compose upstream2
    val connections: immutable.Seq[ProxyStep[_, _, _, _]] =
      (connections1 ++ connections2).foldLeft(immutable.Seq[ProxyStep[_, _, _, _]]()) {
        case (seq, ProxyStep(_, _, _, Seq(conn))) =>
          seq :+ conn
        case (seq, _) =>
          seq
      }
    ProxyStep(metadata1 ++ metadata2, downstream, upstream, connections)
  }

  def run[A, B, C](p: ProxyStep[A, B, B, C])(value: A): C = {
    val fromDownstream = p.downstream(value)
    val result = p.upstream(fromDownstream)
    result
  }
}