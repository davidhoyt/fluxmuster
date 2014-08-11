package com.github.davidhoyt

package object fluxmuster3 {
  import scala.collection._
  import scala.concurrent.Future
  import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

  import scala.language.implicitConversions

  type Linked[-In0, +Out0] =
    Link {
      type In >: In0
      type Out <: Out0
    }

  type Downstream[-In, +Out] = Linked[_ >: In, _ <: Out]
  type Upstream[-In, +Out] = Linked[_ >: In, _ <: Out]

  type BiDi[-DownstreamIn0, +DownstreamOut0, -UpstreamIn0, +UpstreamOut0] =
    BiDirectional {
      type DownstreamIn >: DownstreamIn0
      type DownstreamOut <: DownstreamOut0
      type UpstreamIn >: UpstreamIn0
      type UpstreamOut <: UpstreamOut0
    }

  type SimpleStep[-InDownstream, +OutDownstream, -InUpstream, +OutUpstream] =
    StepLike with StepConnections with BiDi[InDownstream, OutDownstream, InUpstream, OutUpstream]

  type ChainLink = immutable.Seq[Link]
  val EmptyChainLink = immutable.Seq[Link]()

//  type Downstream[-In, +Out] = In => Out
//  type Upstream[-In, +Out] = In => Out
//
//  type SimpleStep[A, B, C, D] = StepLike[A, B, C, D] with Runner[A, B, C, D, Downstream[A, B], Upstream[C, D]] with StepConnections
//
//  type Connections = immutable.Seq[SimpleStep[_, _, _, _]]
//  val EmptyConnections = immutable.Seq[SimpleStep[_, _, _, _]]()

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  implicit class LinkEnhancements[In0, Out0](val link: Link { type In = In0; type Out = Out0 }) extends AnyVal {
    def toLinked: Linked[In0, Out0] =
      link.asInstanceOf[Linked[In0, Out0]]
  }

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Linked[In, Out] =
      Link(fn)

    def link[OtherIn, OtherOut](other: Link { type In = OtherIn; type Out = OtherOut })(implicit proofMyOutputCanBeOtherIn: Out => other.In, tIn: TypeTagTree[In], tOut: TypeTagTree[Out], tOtherOut: TypeTagTree[OtherOut]): Linked[In, OtherOut] =
      toLink(tIn, tOut).andThen(other)(proofMyOutputCanBeOtherIn)
  }

  implicit def functionToLink[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Linked[In, Out] =
    fn.toLink(tIn, tOut)
}
