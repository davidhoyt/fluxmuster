package com.github.davidhoyt

package object fluxmuster {
  import scala.language.existentials
  import scala.language.implicitConversions

  type LinkAny = Link[_, _]
  implicit def toLinkAny[In, Out](link: Link[In, Out]): LinkAny = link.asInstanceOf[LinkAny]

  def typeTagTreeOf[T](implicit ttt: TypeTagTree[T]) = TypeTagTree.typeTagTreeOf[T](ttt)
}
