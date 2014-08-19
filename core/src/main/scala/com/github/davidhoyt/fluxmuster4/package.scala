package com.github.davidhoyt

package object fluxmuster4 {
  import scala.collection._
  import scala.concurrent.{ExecutionContext, Future}
  import com.github.davidhoyt.fluxmuster.TypeTagTree

  import scala.language.higherKinds
  import scala.language.implicitConversions

  type Downstream[In, Out] =
    Link[In, Out]

  type Upstream[In, Out] =
    Link[In, Out]

  type ChainableLink =
    Link[_, _]

  type ChainLink = immutable.Seq[ChainableLink]
  val EmptyChainLink = immutable.Seq[ChainableLink]()

  type FnChainLink =
    (ChainableLink, ChainLink, ChainLink) => ChainLink

  implicit object IdentityConverter {
    def apply[F[_]] = new Converter[F, F] {
      override implicit def apply[A](a: F[A]): F[A] = a
    }
  }

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  implicit class LinkEnhancements[In, Out](val link: Link[In, Out]) extends AnyVal {
    def toLink: Link[In, Out] =
      link
  }

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(name: String)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
      Link(name)(fn)

    def toLink(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
      Link(fn)

    def link[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit proofMyOutputCanBeOtherIn: Out => OtherIn, tIn: TypeTagTree[In], tOut: TypeTagTree[Out], tOtherOut: TypeTagTree[OtherOut]): Link[In, OtherOut] =
      toLink(tIn, tOut).andThen(other)(proofMyOutputCanBeOtherIn)

//    def toStep(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Step[In, Out, Out, Out] =
//      Step.downstream(fn)
  }

  implicit def functionToLink[In, Out](fn: In => Out)(implicit tIn: TypeTagTree[In], tOut: TypeTagTree[Out]): Link[In, Out] =
    fn.toLink(tIn, tOut)
}
