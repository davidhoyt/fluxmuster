package com.github.davidhoyt

package object fluxmuster3 {
  import scala.collection._
  import scala.concurrent.Future
  import com.github.davidhoyt.fluxmuster.TypeTagTree

  import scala.language.implicitConversions

  type Linked[In, Out] =
    Link[In, Out]

  type Downstream[In, Out] =
    Linked[In, Out]

  type Upstream[In, Out] =
    Linked[In, Out]

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

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  implicit class LinkEnhancements[In0, Out0](val link: Link[In0, Out0]) extends AnyVal {
    def toLinked: Linked[In0, Out0] =
      link.asInstanceOf[Linked[In0, Out0]]
  }

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Linked[In, Out] =
      Link(fn)

    def link[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit proofMyOutputCanBeOtherIn: Out => OtherIn, tIn: TypeTagTree[In], tOut: TypeTagTree[Out], tOtherOut: TypeTagTree[OtherOut]): Linked[In, OtherOut] =
      toLink(tIn, tOut).andThen(other)(proofMyOutputCanBeOtherIn)
  }

  implicit def functionToLink[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Linked[In, Out] =
    fn.toLink(tIn, tOut)
}
