package com.github.davidhoyt

package object fluxmuster3 {
  import scala.collection._
  import scala.concurrent.Future
  import com.github.davidhoyt.fluxmuster.TypeTagTree

  import scala.language.implicitConversions

  type Downstream[In, Out] =
    Link[In, Out]

  type Upstream[In, Out] =
    Link[In, Out]

  type BiDi[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    BiDirectional[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]

  type ChainableLink =
    Link[_, _]

  type ChainableBiDi =
    BiDirectionalLike[_, _, _, _] with BiDirectionalRun[_, _, _, _] with BiDirectionalChaining with Named

  type ChainLink = immutable.Seq[ChainableLink]
  val EmptyChainLink = immutable.Seq[ChainableLink]()

  type ChainBiDi = immutable.Seq[ChainableBiDi]
  val EmptyChainBiDi = immutable.Seq[ChainableBiDi]()

  type FnChainLink =
    (ChainableLink, ChainLink, ChainLink) => ChainLink

  type FnChainBiDi =
    (ChainableBiDi, ChainBiDi, ChainBiDi) => ChainBiDi

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  implicit class LinkEnhancements[In, Out](val link: Link[In, Out]) extends AnyVal {
    def toLink: Link[In, Out] =
      link
  }

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
      Link(fn)

    def link[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit proofMyOutputCanBeOtherIn: Out => OtherIn, tIn: TypeTagTree[In], tOut: TypeTagTree[Out], tOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
      toLink(tIn, tOut).andThen(other)(proofMyOutputCanBeOtherIn)
  }

  implicit def functionToLink[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    fn.toLink(tIn, tOut)
}
