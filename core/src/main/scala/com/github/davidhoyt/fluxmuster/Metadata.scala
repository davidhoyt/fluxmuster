package com.github.davidhoyt.fluxmuster

trait Metadata {
  val name: String
  val typeAcceptDownstream: TypeTagTree[_]
  val typeMappedDownstream: TypeTagTree[_]
  val typeAcceptUpstream: TypeTagTree[_]
  val typeMappedUpstream: TypeTagTree[_]

  def asString: String =
    null

  val defaultShortString =
    s"$name[${typeAcceptDownstream.toShortString}, ${typeMappedDownstream.toShortString}, ${typeAcceptUpstream.toShortString}, ${typeMappedUpstream.toShortString}]"

  lazy val toShortString =
    if (asString eq null)
      defaultShortString
    else
      asString

  override def toString =
    toShortString
}

object Metadata {
  import scala.language.existentials

  private case class Meta(name: String, typeAcceptDownstream: TypeTagTree[_], typeMappedDownstream: TypeTagTree[_], typeAcceptUpstream: TypeTagTree[_], typeMappedUpstream: TypeTagTree[_], override val asString: String) extends Metadata

  def apply(name: String, typeAcceptDownstream: TypeTagTree[_], typeMappedDownstream: TypeTagTree[_], typeAcceptUpstream: TypeTagTree[_], typeMappedUpstream: TypeTagTree[_], asString: String = null): Metadata =
    Meta(name, typeAcceptDownstream, typeMappedDownstream, typeAcceptUpstream, typeMappedUpstream, asString)
}
