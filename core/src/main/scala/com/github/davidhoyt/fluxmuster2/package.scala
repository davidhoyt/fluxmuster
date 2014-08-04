package com.github.davidhoyt

package object fluxmuster2 {
  import scala.collection.immutable
  import scala.concurrent._

  type LinkDownstream[-A, +B] = A => B
  type LinkUpstream[-C, +D] = C => D

  type SimpleStep[A, B, C, D] = StepLike[A, B, C, D] with Runner[A, B, C, D] with StepConnections

  type Connections = immutable.Seq[SimpleStep[_, _, _, _]]
  val EmptyConnections = immutable.Seq[SimpleStep[_, _, _, _]]()


  import scala.language.higherKinds
  import scala.language.implicitConversions

  implicit object FutureLiftOp extends LiftOp[ExecutionContext, Future] {
    def apply[A, D](runner: A => D)(implicit ec: ExecutionContext): A => Future[D] =
      (a: A) =>
        future {
          runner(a)
        }

    def map[A, B](given: Future[A])(fn: A => B)(implicit ec: ExecutionContext): Future[B] =
      given.map(fn)(ec)

    def flatten[A](given: Future[Future[A]])(implicit ec: ExecutionContext): Future[A] = {
      import scala.util._
      val p = Promise[A]()
      given.onComplete {
        case Success(next) =>
          p.completeWith(next)
        case Failure(t) =>
          p.failure(t)
      }
      p.future
    }
  }

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  //Courtesy Miles Sabin:
  //  http://www.chuusai.com/2012/05/10/shapeless-polymorphic-function-values-2/

  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  implicit def polyToMono2[G[_], T](f: Id -> G): T => G[T] = f(_)
  implicit def polyToMono3[F[_], T](f: F -> Id): F[T] => T = f(_)
  implicit def polyToMono4[T](f: Id -> Id) : T => T = f[T](_)
  implicit def polyToMono5[F[_], G, T](f: F -> Const[G]#λ) : F[T] => G = f(_)
  implicit def polyToMono6[G, T](f: Id -> Const[G]#λ) : T => G = f(_)

  implicit object IdentityConverter extends (Id -> Id) {
    implicit def apply[A](a: Id[A]): Id[A] = a
  }
}
