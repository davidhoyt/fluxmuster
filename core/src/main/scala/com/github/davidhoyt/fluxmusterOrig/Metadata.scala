package com.github.davidhoyt.fluxmuster

import scala.collection._

trait Metadata {
  val name: String
  val typeAcceptDownstream: TypeTagTree[_]
  val typeMappedDownstream: TypeTagTree[_]
  val typeAcceptUpstream: TypeTagTree[_]
  val typeMappedUpstream: TypeTagTree[_]
  val metadata: immutable.Seq[Metadata]

  def lifted: Boolean =
    metadata.nonEmpty

  def asString: String =
    null

  val defaultShortString =
    s"$name[${typeAcceptDownstream.toShortString}, ${typeMappedDownstream.toShortString}, ${typeAcceptUpstream.toShortString}, ${typeMappedUpstream.toShortString}]"

  lazy val toShortString = {
    val s = asString
    if (s eq null)
      defaultShortString
    else
      s
  }

  override def toString =
    toShortString
}

object Metadata {
  import scala.language.existentials

  private case class Meta(name: String, typeAcceptDownstream: TypeTagTree[_], typeMappedDownstream: TypeTagTree[_], typeAcceptUpstream: TypeTagTree[_], typeMappedUpstream: TypeTagTree[_], metadata: immutable.Seq[Metadata], override val asString: String) extends Metadata

  def unapply(meta: Metadata): Option[(String, TypeTagTree[_], TypeTagTree[_], TypeTagTree[_], TypeTagTree[_], Boolean, immutable.Seq[Metadata])] =
    PartialFunction.condOpt(meta) {
      case m => (m.name, m.typeAcceptDownstream, m.typeMappedDownstream, m.typeAcceptUpstream, m.typeMappedUpstream, m.lifted, m.metadata)
    }

  //TODO: Remove "lifted" from this signature and everywhere else it's used
  def apply(name: String, typeAcceptDownstream: TypeTagTree[_], typeMappedDownstream: TypeTagTree[_], typeAcceptUpstream: TypeTagTree[_], typeMappedUpstream: TypeTagTree[_], lifted: Boolean = false, metadata: immutable.Seq[Metadata] = immutable.Seq(), asString: String = null): Metadata =
    Meta(name, typeAcceptDownstream, typeMappedDownstream, typeAcceptUpstream, typeMappedUpstream, metadata, asString)
}
