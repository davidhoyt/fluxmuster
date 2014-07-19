package com.github.davidhoyt.fluxmuster

import scala.collection._

trait Metadata {
  val name: String
  val typeAcceptDownstream: TypeData[_]
  val typeMappedDownstream: TypeData[_]
  val typeAcceptUpstream: TypeData[_]
  val typeMappedUpstream: TypeData[_]
  val additionalTypes: immutable.Seq[TypeData[_]] =
    immutable.Seq.empty
}

object Metadata {

  private case class Meta(name: String, typeAcceptDownstream: TypeData[_], typeMappedDownstream: TypeData[_], typeAcceptUpstream: TypeData[_], typeMappedUpstream: TypeData[_], override val additionalTypes: immutable.Seq[TypeData[_]] = immutable.Seq.empty) extends Metadata

  def apply(name: String, typeAcceptDownstream: TypeData[_], typeMappedDownstream: TypeData[_], typeAcceptUpstream: TypeData[_], typeMappedUpstream: TypeData[_], additionalTypes: immutable.Seq[TypeData[_]] = immutable.Seq.empty) =
    Meta(name, typeAcceptDownstream, typeMappedDownstream, typeAcceptUpstream, typeMappedUpstream, additionalTypes)
}
