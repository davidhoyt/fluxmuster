package com.github.davidhoyt.fluxmuster

trait Metadata {
  val name: String
  val typeAcceptDownstream: TypeTagTree[_]
  val typeMappedDownstream: TypeTagTree[_]
  val typeAcceptUpstream: TypeTagTree[_]
  val typeMappedUpstream: TypeTagTree[_]

  def toShortString =
    s"$name[${typeAcceptDownstream.toShortString}, ${typeMappedDownstream.toShortString}, ${typeAcceptUpstream.toShortString}, ${typeMappedUpstream.toShortString}]"
}

object Metadata {
  import scala.language.existentials

  private case class Meta(name: String, typeAcceptDownstream: TypeTagTree[_], typeMappedDownstream: TypeTagTree[_], typeAcceptUpstream: TypeTagTree[_], typeMappedUpstream: TypeTagTree[_]) extends Metadata

  def apply(name: String, typeAcceptDownstream: TypeTagTree[_], typeMappedDownstream: TypeTagTree[_], typeAcceptUpstream: TypeTagTree[_], typeMappedUpstream: TypeTagTree[_]): Metadata =
    Meta(name, typeAcceptDownstream, typeMappedDownstream, typeAcceptUpstream, typeMappedUpstream)
}
